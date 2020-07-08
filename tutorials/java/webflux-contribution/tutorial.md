---
title: Fixing array type look-up on r2dbc-postgresql, a reactive library from Spring Framework
published: 2020-07-08
tags: java, spring, webflux, r2dbc-postgresql
language: java
author-name: José Luis León
twitter-profile: JoseLeonT
github-profile: JoseLion
description: The programming world is adopting practices from the functional and reactive programming paradigms. Here, we’ll review how we can integrate a relational database like PostgreSQL with Spring WebFlux while keeping the reactive design. Then we’ll see how we contributed to r2dbc-postgresql to solve a datatype decoding issue related to array-like types.
---

Reading Roman Elizarov's article "[Functional Programming is on the rise](https://medium.com/@elizarov/functional-programing-is-on-the-rise-ebd5c705eaef) [1]," we can tell that the programming world is adopting practices from the functional and reactive programming paradigms. At Stack Builders, we encourage functional programming practices for creating reliable applications. These practices allow us to focus on implementing the core business logic instead of more frequently fixing bugs. Some of the contributions that we've made include a [couple of pull requests](https://github.com/r2dbc/r2dbc-postgresql/pulls?q=is%3Apr+author%3AJoseLion+is%3Amerged) in the open-source project `r2dbc-postgresql`, a reactive/non-blocking alternative for JDBC in Spring WebFlux.

Pivotal's Spring WebFlux is an asynchronous, non-blocking web framework built on the reactive design principles to handle massive numbers of concurrent connections. This framework introduces a lambda-based, lightweight, and functional programming model which takes advantage of the addition of lambda expressions in Java 8. WebFlux is based on [ProjectReactor](https://projectreactor.io/), which directly interacts with Java functional API, CompletableFuture, Stream, and Duration.

Spring MVC (the blocking-stack web framework) has a set of libraries for managing data, called **Spring Data Repositories**. They allow connections to relational and non-relational databases (SQL and NoSQL). Looking into Spring's documentation we can find that Spring WebFlux has its counterpart, called **Spring Data Reactive Repositories**. However, these only offer implementations for non-relational databases. In this blog post, we’ll briefly review how we can integrate a relational database like PostgreSQL with Spring WebFlux while keeping the reactive design. Then we’ll see how we contributed to `r2dbc-postgresql` to solve a datatype decoding issue related to array-like types.

## Relational databases with reactive-stack

To connect our application to PostgreSQL, we'll have to look into the R2DBC project (Reactive Relational Database Connectivity), which brings us to [spring-data-r2dbc](https://github.com/spring-projects/spring-data-r2dbc) and [r2dbc-postgresql](https://github.com/r2dbc/r2dbc-postgresql) libraries. These two dependencies allow us to create a reactive connection to a PostgreSQL database. As we are using Spring Boot, we can configure our connection by just extending the configuration from `AbstractR2dbcConfiguration` and overriding the `connectionFactory` method:

```java
@Configuration
public class ApplicationConfiguration extends AbstractR2dbcConfiguration {

  @Bean
  @Override
  public ConnectionFactory connectionFactory() {
    final PostgresqlConnectionConfiguration connectionConfig = PostgresqlConnectionConfiguration.builder()
      .database("<db-name>")
      .host("<host-url>")
      .password("<password>")
      .port(5432)
      .username("<username>")
      .build();
  
    return new PostgresqlConnectionFactory(connectionConfig);
  }
}
```

This will give our Spring Boot application access to a DatabaseClient connection object, which we can inject anywhere using the `@Autowired` annotation. With this client, we can exchange operations with our database in a non-blocking reactive way and with a fluent and descriptive API. An alternative would be to use the popular repository mechanism by adding the `@EnableR2dbcRepositories` annotation to our main Spring Boot class, but we will go through that approach in another post.

Then, we would want to map our database table to Java classes using the annotations provided by `org.springframework.data`. To do so, let’s use one of the most common entities an application will have: Account. This table stores all users’ account information, including fields like username, password, roles, and last access. The SQL statement to create our table looks like this:

```sql
CREATE TABLE public.account(
  id serial NOT NULL,
  username text NOT NULL,
  password text NOT NULL,
  roles text[],
  last_access timestamp with time zone,
  PRIMARY KEY (id)
);
```

And our Java class for the account table should be as follows:

```java
@AllArgsConstructor
@Builder(toBuilder = true)
@Getter
public class Account {

  @Id
  @NotNull
  private final Long id;

  @NotNull
  private final String username;

  @NotNull
  private final String password;

  private final String[] roles;

  private final ZonedDateTime lastAccess;
}
```

**Note:** We're using Project Lombok annotations to reduce the boilerplate of constructor, getters and the builder pattern.

And just like that, we're all set! Now, to test this code let's use [JUnit Jupiter](https://junit.org/junit5/) to create a test case that saves an account in our database. Then we’ll find it and verify its existence as a Java object:

```java
@SpringBootTest
@DisplayNameGeneration(DisplayNameGenerator.ReplaceUnderscores.class)
public class PersonTest {
  
  @Autowired
  private DatabaseClient client;
  
  @Test
  void account_persistance_test() {
    final Account account = Account.builder()
      .username("user@test.com")
      .password("1234")
      .roles(new String[] { "TEST_ROLE" })
      .build();

    client.insert()
      .into(Account.class)
      .using(account)
      .fetch()
      .rowsUpdated()
      .flatMap(updatedRows ->
        client.select()
          .from(Account.class)
          .matching(where("username").is("user@test.com"))
          .fetch()
          .one()
      )
      .as(StepVerifier::create)
      .assertNext(found -> {
        assertThat(found.getId()).isNotNull();
      })
      .verifyComplete();
  }
}
```

This process is simple, right? By asserting that the id field exists, we can ensure that the new account was inserted into the database table with an auto-generated serial ID. Unfortunately, when we tried the first time, the test failed with the following error:

```
Caused by: java.lang.IllegalArgumentException: Cannot decode value of type java.lang.Object
  at io.r2dbc.postgresql.codec.DefaultCodecs.decode(DefaultCodecs.java:96) ~[r2dbc-postgresql-1.0.0.M7.jar:na]
  at io.r2dbc.postgresql.PostgresqlRow.get(PostgresqlRow.java:83) ~[r2dbc-postgresql-1.0.0.M7.jar:na]
  at io.r2dbc.spi.Row.get(Row.java:46) ~[r2dbc-spi-1.0.0.M7.jar:na]
  at org.springframework.data.r2dbc.convert.MappingR2dbcConverter$RowParameterValueProvider.getParameterValue(MappingR2dbcConverter.java:426) ~[spring-data-r2dbc-1.0.0.M2.jar:1.0.0.M2]
  ... 81 common frames omitted
```

## Debug and identify

It may be hard to identify why this exception is raised. The message says "Cannot decode value of type java.lang.Object", so we know there's a value that cannot be decoded from the database table into the Java object, but which one? The message also says the value is of type Object, so the type won't let us identify the failing field either. Therefore, we debugged the `r2dbc-postgresql` library. This showed the error was on a field with type `String[]`.

After looking into `r2dbc-postgresql` source code, it became evident where the error might be. The decoding process is generic and relies on the `java.lang.Class` API, using a method like `isAssignableFrom(..)` to identify the actual type of the value to be decoded. In the case of arrays the library will need to get the component type to know how to decode each element, so looking into the file `io.r2dbc.postgresql.codec.AbstractArrayCodec.java` which is the contract for array-like codecs, we came across these two methods:

```java
boolean isTypeAssignable(Class<?> type) {
  Assert.requireNonNull(type, "type must not be null");

  if (!type.isArray()) {
    return false;
  }

  return getBaseComponentType(type).equals(this.componentType);
}

private static Class<?> getBaseComponentType(Class<?> type) {
  Class<?> t = type;

  while (t.isArray()) {
    t = t.getComponentType();
  }

  return t;
}
```

These methods will check if the type of the value to be decoded is indeed an array, and if so which is the base type of the array. But then again, this is processed over generic Object types, and at the time the method `isArray()` is called, it will always return false. There's a workaround though, we can just use the method `getComponentType()` to verify if the type is an array and which sub-type this array is. JavaDoc for this method states:

> Returns the Class representing the component type of an array. If this class does not represent an array class this method returns null.

We would know that the type is not an array if this method returns null, so we can refactor the library methods to this:

```java
boolean isTypeAssignable(Class<?> type) {
  Assert.requireNonNull(type, "type must not be null");

  return this.componentType.equals(getBaseComponentType(type));
}

private static Class<?> getBaseComponentType(Class<?> type) {
  Class<?> t = type;

  while (t.getComponentType() != null) {
    t = t.getComponentType();
  }

  return t;
}
```

## Conclusion

This code was merged into the master branch and was shipped as part of the first release candidate of the library. Going from version `0.8.0.RC1`, we can have unidimensional array types in our PostgreSQL database, and use them in our Java entities as `String[]`, `Integer[]`, `Long[]` or `Short[]`.

A special thanks to Pivotal developers who took the time to check the issue and the pull request. We encourage you to contribute to this library as well.

## References

[1] Elizarov R. (May 4, 2019). Functional Programming is on the rise. Medium Programming Blog. https://medium.com/@elizarov/functional-programing-is-on-the-rise-ebd5c705eaef
