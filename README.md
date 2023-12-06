# createfile #

createfile

Create a file via command line arguments.

Writes a line to the output file for every argument provided. Wrap the arguments for lines with spaces.

`createfile` is compiled statically and should have no library dependencies.

## usage ##

createfile file contents

```
$ createfile \
    ./.env \
    "HOST_PORT = 8888" \
    "CLIENT_PORT = 80"
```

The example will create an `.env` file, or **overwrite** it if it already exists.

```
HOST_PORT = 8888
CLIENT_PORT = 80
```

## compiling ##

Install `stack` and run `build`, `test`, and `install`.

`createfile` requires `glibc-static` (may be `glibc-devel-static` depending on your distro) to compile.

### clone ###

```
$ git clone https://github.com/frostbane/createfile
```
### build ###

Change directory to `createfile` and `build`.

```
$ stack build
```
### test ###

```
$ stack test
```

### install ###

Install it so you can use it anywhere.

```
$ stack install
```
