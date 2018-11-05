# Getting Started

## Initialising the project

We will begin in an empty folder and initialise a PureScript project using pulp. This will create a project skelleton and install the prelude and two additional packages.

```
pulp init
```

Let's verify that your installation works by running the initialised project using pulp. This command will be the single command that we use to start our server but for now it should just print a string to the console.

```
pulp run
```

## Installing PureScript dependencies

Next, we will install some dependencies that we will be using in this tutorial. To create our GraphQL API we will of cause use the `purescript-graphql` package. The package contains all modules that we need to decribe a GraphQL Schema.

The `purescript-aff` package defines the `Aff` monad. The `Aff` monad is used to describe asynchronious effects. When writing APIs we usually deal with a lot of asynchronious IO (read and write). For example we might want to read records from database and then return them to the client.

GraphQL itself does not specify the transportation layer of the client server communication. In our case we will simply go with HTTP. The `purescript-httpure` package will help us build a web server that can respond to HTTP post requests that contain a GraphQL query. PureScript does not support subscriptions yet, otherwise we would also need to provide a technology that allows the server to _push_ data to the client (e.g. Web Sockets). GraphQL query results are encoded in JSON. We will need the packages `purescript-argonaut` and `purescript-foreign-object` simply to deal with the return value of a query execution and to encode the value to a string that we can return to the client.

We will get to know all of these packages in the following chapters. Don't feel lost! You can simply go ahead and run this command in your project directory, that we just created, and you are good to go.

```
bower install --save purescript-prelude purescript-console purescript-effect purescript-httpure purescript-argonaut purescript-foreign-object
```

Last but not least we will create a stateful store that will serve as an in-memory database. In a real project you would use a package to connect to your database. This means these packages are just for the sake of this tutorial and you can uninstall them once you have connected a different data source.

```
bower install --save purescript-refs purescript-uuid
```

## Installing JavaScript dependencies

Unfortunately PureScript uses _Bower_ to distribute packages, while all JavaScript dependencies are distributed via NPM these days. This means your PureScript dependencies cannot install their JavaScript dependencies automatically. The fastest way to install the JavaScript dependencies is to create a `package.json` file in your project directory with the following content:

```json
{
  "name": "purescript-graphql-example",
  "version": "1.0.0",
  "dependencies": {
    "graphql": "^14.0.2",
    "uuid": "^3.3.2",
    "uuid-validate": "0.0.3"
  }
}
```

Now all you have to do is run `npm install` in your console and you should be good to go! Again, feel free to remove the _UUID_ dependencies when you no longer make use of the `purescript-uuid` package in the future. Your project directory should now contain the following files and folders:

```
bower.json
bower_components
node_modules
output
package-lock.json
package.json
src
test
```
