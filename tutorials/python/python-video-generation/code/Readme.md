# Local environment set up

---

## Before Getting Started

Make sure you have
[Cairo](https://pycairo.readthedocs.io/en/latest/getting_started.html) installed
locally. Follow the instruction in the link.

## Install Python 3

### Linux and OSX

Using version manager [asdf-vm](https://asdf-vm.com/#/core-manage-asdf-vm?id=install-asdf-vm):

##### Install [Python Plugin](https://github.com/danhper/asdf-python):

```bash
$ asdf plugin-add python https://github.com/danhper/asdf-python.git
```

##### Install Python:

Python version is specified in `.tools-versions` file.

```bash
$ asdf install
```

**Note:** This can be installed using package managers as well, the instruction
may vary according to your package manager:

- In Ubuntu:

```bash
sudo apt-get install python3
```

- In OSx:

```bash
brew install python3
```

### Windows

Using package manager [Chocolatey](https://chocolatey.org/)

```bash
C:\> choco install python3
```

#### Install dependencies

From root project directory run:

```bash
$ pipenv install
```

#### Enable your virtual environment

From root project folder, enable virtual env typing

```bash
pipenv shell
```

#### To generate the video

With pipenv virtualenv enabled and from the root project folder run

```bash
python python_video.py
```

**Note:** It's possible to execute the script directly with `pipenv` using:

```bash
pipenv run python python_video.py
```

#### Disable Virtual Environment

To Disable it type `exit`
