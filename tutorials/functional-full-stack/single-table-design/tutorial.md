# The single table design. What is it?

First, to give the context to what this blog post is talking about, “Serverless” is the key. If you are building a serverless application, or just interested in how the serverless world works one way or another you are going to end up asking yourself how do you handle the data, then find something about NoSQL and finally end up looking for better or different ways to improve your serverless architecture. Well, the single table design is one of them.

The single table design is used in NoSQL databases, and it has come more popular in the past years especially using it with AWS DynamoDB. NoSQL databases can be flexible, highly scalable, have extremely high performance but they sacrifice some benefits for example the beautiful ‘join’ clause and the normalization that exist on relational databases.

There are no joins and the alternatives to a join will cost extra money because they burn the CPU. So how can this be solved? Using the single table design, the data is pre-joined creating items collections that help to get our data faster and efficiently. It literally turns all the data that exists in many relational tables into one.

Let’s check some of the benefits of this approach.

### Less and more efficient requests :

The main one is to decrease the number of requests made to retrieve the data. How is this achieved? Well, let’s make a comparison.

You can use a NoSql DB with a multi-table design, exactly as you would use a relational database. This means that if you want to retrieve the information to get a simple request like getting the orders of a user you will have to make two requests. One for the user table, and another one for the orders table as you can see in this graphic.

But if you use the single table design we will be pre-joining the data and there will be only a need for a single and more efficient request, as you can see in this graphic.

### Save us money:

As you probably know the RDBS services were developed specially to reduce the costs of storage, it optimizes the way to store our data, but the CPU does suffer from difficult queries. Storage is really cheap in comparison to the processing. The trick is to avoid charging the processor with heavy loads of joins.

### Centralizing and monitoring:

When working with Dynamo all the metrics and data can be monitored for each table. So with the multi-table design, you will need to set up the monitors for all the tables created, and set alarms, and boards, and any other data that you would like to check.

And with the single table approach, you only have to take care of the health of one table.

## Changing the mindset:

Now that you have an idea of what the single table design is, you have to acknowledge that it can be a bit hard to change the mindset. But if you are looking for a really affordable and highly scalable design it’s worth researching a bit more about it. I can recommend Rick Houlihan and Alex Debree material, they both have been precursors and have amazing videos explaining this approach and how to use it.

**The pet project. How to model a single table design:**

To explain how and why to use the single table design, there is nothing better than an example. The steps below describe the recommended way to model data for NoSql with a single table design.

### 1. Understand the application you are building and its use cases.

It’s crucial to understand the application to be built and define the use cases that it is going to have. For single table design is even more important because the model is hard to update on the run and to pre-join the data and be able to retrieve it according to the needs, it is important to previously detect the data to be used.

So to do that, a good exercise is to answer these questions:

- What kind of application is being built?
  - Start digging with the customer or with whoever is going to give the information for the app and start asking questions until it is completely understood how the data should be handled. For this example:
    - What is the application for?: To receive user requests for quotations.
    - What data the users need to see: Which products the client has requested, the categories of the products, the clients, all the quotations. The products by make, category, and quotation status. Etc, etc The objective is to start craving all the information the app will need to present and this will be turned into the access patterns later.
    - What reports do you need to receive:
- How does the data relate to each other? Ok, here the best approach is to create a relational data model, to have visual representation:

### 2. Access patterns

All the answers retrieved from the client about how the app is going to work and which data is going to be displayed are useful for creating the access patterns.

The access patterns define how the single table is going to handle the exact information. The way to define the access patterns of the data is to write them down. As shown in the following example:

<table>
  <tr>
   <td>Get all products
   </td>
  </tr>
  <tr>
   <td>Get product by id
   </td>
  </tr>
  <tr>
   <td>Get products by category
   </td>
  </tr>
  <tr>
   <td>Get products by make
   </td>
  </tr>
  <tr>
   <td>Get products by quotation status
   </td>
  </tr>
  <tr>
   <td>
   </td>
  </tr>
  <tr>
   <td>Get category by id
   </td>
  </tr>
  <tr>
   <td>Get category by make
   </td>
  </tr>
  <tr>
   <td>Get all categories
   </td>
  </tr>
  <tr>
   <td>
   </td>
  </tr>
  <tr>
   <td>Get make by id
   </td>
  </tr>
  <tr>
   <td>Get all makes
   </td>
  </tr>
  <tr>
   <td>
   </td>
  </tr>
  <tr>
   <td>Get all clients
   </td>
  </tr>
  <tr>
   <td>Get client quotations by source
   </td>
  </tr>
  <tr>
   <td>
   </td>
  </tr>
  <tr>
   <td>Get quotations and client info by client
   </td>
  </tr>
  <tr>
   <td>Get quotations by date
   </td>
  </tr>
  <tr>
   <td>Get quotations by status
   </td>
  </tr>
  <tr>
   <td>Get quotations by make
   </td>
  </tr>
  <tr>
   <td>Get quotations by product
   </td>
  </tr>
  <tr>
   <td>Get quotations by category
   </td>
  </tr>
  <tr>
   <td>Get quotations by status and date
   </td>
  </tr>
  <tr>
   <td>Get quotations by status, date, and product
   </td>
  </tr>
  <tr>
   <td>Get quotations by status, date, and category
   </td>
  </tr>
</table>

As you can see the list above gives a clear idea of how the data is going to be modeled in the design. And which is the data that should be pre-joined, for example, the access pattern “Get quotations by the client” means that the table quotation and client should be pre-joined in order to get the data in one query.

Also, it shows all the tables to take into account in the model. This would be easy to recognize in relational databases, but in access patterns, they are represented with “Get all the products”.

So this list must be well defined because it is extremely important to cover all the use cases for the application.

### 3.- Data modeling

Now that the access patterns are well defined, the next step is to model the data into the single table design.

It is recommended to use any modeling tool, for example, NoSQL WorkBench is an AWS tool that you can get [here](https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/workbench.settingup.html) for free, and you can even connect to your database and start making queries for testing purposes.

**Partition Key, Sort Key, Secondary Indexes?**

To model the data, it is important to know that the single table design is formed by three important keys.

1. **PK**: The partition key, is the unique required identifier of the items in the table. It does not let you create relations with other tables. Is just there to uniquely identify the items across the table. It can be a string, number, or binary value.

<table>
  <tr>
   <td colspan="2" >
Example of a single table only with Partition Keys
<p>
Table name: Quotations
   </td>
  </tr>
  <tr>
   <td>Partition Key
   </td>
   <td>name
   </td>
  </tr>
  <tr>
   <td>PRODUCT#W183JD83JK
   </td>
   <td>Mustang
   </td>
  </tr>
  <tr>
   <td>CATEGORY#E823JD08
   </td>
   <td>Sports car
   </td>
  </tr>
  <tr>
   <td>CLIENT#JD92KD93
   </td>
   <td>John Doe
   </td>
  </tr>
  <tr>
   <td>MAKE#W183JD88JE
   </td>
   <td>Ford
   </td>
  </tr>
</table>

The example above shows exactly how a single table design might look using only a partition key and an additional field ‘name’. There are several entities in the same table, but they can be queried by the partition key. So for example if you want to retrieve the product with the ID ‘PRODUCT#W183JD83JK’ can do:

    SELECT * FROM Quotations WHERE PK = 'PRODUCT#W183JD83JK'


    The partition key helps us cover the ‘Get by id’ access patterns.

**NOTE**: The SQL statement above is just a reference. In NoSQL and DynamoDB, PartiSQL is used, but the best practice is to query using the SDK. So for example in JS the queries will look like this:

Now, what about the other access patterns? For example ‘Get all products’ or ‘Get all products by make’ here is where the sort key comes to help.

2. **SK**: The sort key, this key is optional but helpful. Its function is to help sort and search through the table. So, to be able to get all the products or all the makes we can create a sort key that contains the type of the item to be able to query them. Like this:

<table>
  <tr>
   <td colspan="3" >
Example of a single table only with Partition Key and Sort Key
<p>
Table name: Quotations
   </td>
  </tr>
  <tr>
   <td>Partition Key
   </td>
   <td>Sort Key
   </td>
   <td>name
   </td>
  </tr>
  <tr>
   <td>PRODUCT#W183JD83JK
   </td>
   <td>PRODUCT
   </td>
   <td>Mustang
   </td>
  </tr>
  <tr>
   <td>PRODUCT#W1444283JK
   </td>
   <td>PRODUCT
   </td>
   <td>A4
   </td>
  </tr>
  <tr>
   <td>PRODUCT#W1444283JK
   </td>
   <td>PRODUCT
   </td>
   <td>A5
   </td>
  </tr>
  <tr>
   <td>CLIENT#JD92KD93
   </td>
   <td>CLIENT
   </td>
   <td>John Doe
   </td>
  </tr>
  <tr>
   <td>MAKE#W183JD88JE
   </td>
   <td>MAKE
   </td>
   <td>Ford
   </td>
  </tr>
</table>

If you notice it, the access pattern “Get all products” hasn’t been resolved yet only by the use of a sort key. Here is where the global secondary index is introduced because all the queries require a partition key, and the partition key doesn’t allow you to make any conditions, it just searches by id.

When a sort key exists, it lets you query with conditions like `BEGIN WITH`, `EQUAL`, `LESS THAN`, `HIGHER THAN`. But if the partition key is always different, it won’t work to solve the access patterns ‘Get all products’ or ‘Get all products by make’. So here is where the GSI comes to help.

3. **GSI**: The global secondary indexes help to project, search and order the data according to the partition key and secondary key we choose for this index. To make this clear with an example, to solve the ‘Get all products’ or ‘Get all makes’ access patterns a reverse key GSI can be created like this:

<table>
  <tr>
   <td colspan="3" >
Example of a single table only with Partition Key and Sort Key
<p>
Table name: Quotations
   </td>
  </tr>
  <tr>
   <td>Partition Key
   </td>
   <td>Sort Key
   </td>
   <td>name
   </td>
  </tr>
  <tr>
   <td>PRODUCT
   </td>
   <td>PRODUCT#W183JD83JK
   </td>
   <td>Mustang
   </td>
  </tr>
  <tr>
   <td>PRODUCT
   </td>
   <td>PRODUCT#W1444283JK
   </td>
   <td>A4
   </td>
  </tr>
  <tr>
   <td>PRODUCT
   </td>
   <td>PRODUCT#W1444283JK
   </td>
   <td>A5
   </td>
  </tr>
  <tr>
   <td>CLIENT
   </td>
   <td>CLIENT#JD92KD93
   </td>
   <td>John Doe
   </td>
  </tr>
  <tr>
   <td>MAKE
   </td>
   <td>MAKE#W183JD88JE
   </td>
   <td>Ford
   </td>
  </tr>
</table>

Reverse means to project the same data but using the SK as PK, and the PK as SK to be able to sort. This strategy solves the access pattern ‘Get all products’:

    SELECT * FROM Quotations WHERE PK = 'PRODUCT'

This not only solves those access patterns. This will also solve ‘Get all clients’, and ‘Get all makes’ and others. To make this easier and more ‘real life’ below you can see the actual table with all the example data of the pet project:

Now, applying the GSI: Reverse

And what about all the other access patterns? To be able to solve them, more GSIs can be created and more fields that can help to query the information according to what it’s needed. That is why it is so important to define the access patterns first. Because it is the only way to know what strategies, GSIs, and keywords to use.

For the pet project, the following additional fields and GSIs were created:

**GSI** **Relations**: This one uses the SK as PK and “auxiliar” as SK. This makes it easy to solve the access patterns like: Get all products by make, by category, get all quotations by make, category, product.

As you can see the “auxiliar” field has values like Audi#A4, or MAKE#123#CATEGORY#123#PRODUCT#123. This also has a purpose, which is to be able to query using CONTAINS or BEGINS with.

To have all the keys in one field makes it easy to solve the access patterns described above.

**GSI DateStatus:** This index helps to get the Quotations by date and status. A specific field “dateStatus” was created to join the date and status. That way is easier to query by a date range and the status of the quotations.

**GSI Clients-Quotations:** This last index serves the purpose of joining tables. Clients and Quotations. Because one access pattern is ”Get all client quotations and client information”. So in order to avoid making two queries, this GSI allows you to retrieve both tables in one query as you can see in the image below.

## Wrapping up:

This blog post reviewed the basics of the Single table design, the reason for its existence, benefits, and downsides.

Probably one of the most difficult parts of using this approach is changing the mindset and the learning curve. But again is a good practice for OLTP applications and specially focused on serverless architectures.

It’s important to know that building a single table design can be difficult and you will need several iterations until you meet the objective of covering all the access patterns.

Also, the pet project showed a sneak peek on how to start modeling your data, following the recommended steps, and there are several strategies and approaches able to solve the different situations for the use case.
