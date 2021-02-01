# Stack Builders Tutorials

[![CircleCI](https://circleci.com/gh/stackbuilders/tutorials.svg?style=shield)](https://circleci.com/gh/stackbuilders/tutorials)

At Stack Builders, we consider it our mission not only to develop robust and reliable applications for clients, but to help the industry as a whole by lowering the barrier to entry for technology that we consider important. We hope that you enjoy, and find useful, our tutorials. If you have suggestions for additional tutorials, please feel free to open an issue or PR.

## Contributing

We welcome any help, so if you have good knowledge of Haskell just fork and submit a pull request with your tutorial. Be sure to read our [contribution guidelines](.github/CONTRIBUTING.md) first.

## Report a problem

Found a typo? Please let us know! If you find an error, please report
it by [opening an issue][tutorials-issues-new].

[tutorials-issues-new]: https://github.com/stackbuilders/tutorials/issues/new

## License

* The content of these tutorials is licensed under a Creative Commons
  Attribution-NonCommercial-ShareAlike 4.0 International license
  ([CC BY-NC-SA 4.0](http://creativecommons.org/licenses/by-nc-sa/4.0/))
* The code in these tutorials is licensed under the [MIT License](https://opensource.org/licenses/MIT).

***

## Run tutorials locally

In order to run this repo locally you need to have `stack` installed, for more info you can take a look
[here](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

To run your project locally all you need to do is:

```bash
$ make watch
```

If you ever want to rebuild the project:

```bash
$ make rebuild
```

If you're a stack builder and want to bring all assets to this project, you can make a symbolic link to your
local website's assets directory (assuming that you have that repo), and put it inside the `_site` directory that will be created when
building the project.

```bash
$ cd _site
$ ln -s ../stackbuilders/assets
```


<p align="center">
  <img  src="images/sb-logo.png" />
  </br>
  <b>Lovingly built by <a href="http://stackbuilders.com">Stack Builders</a></b>
</p>
