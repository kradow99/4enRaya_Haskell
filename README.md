# Connect 4 Game

Programa del juego 4 en raya programado en Haskell. Permite a un usuario jugar contra 3 IA's diferentes en un tablero de cualquier tamaño con más de 4 filas y 4 columnas

## Cómo jugar

Compilar el fichero joc.hs:
```
ghc joc.hs
```
Abrir el ejecutable generado:
```
./joc
```
### Prerequisitos

Para poder compilar es necesario instalar el paquete `random`, ya que es necesario importar `System.random` para generar números aleatorios en una de las estrategias del juego.

En Mac:
```
> brew install cabal-install
> cabal update
> cabal install --lib random
```
En Ubuntu:
```
> sudo apt install cabal-install
> cabal update
> cabal install random
```

### Installing

A step by step series of examples that tell you how to get a development env running

Say what the step will be

```
Give the example
```

And repeat

```
until finished
```

End with an example of getting some data out of the system or using it for a little demo

## Running the tests

Explain how to run the automated tests for this system

### Break down into end to end tests

Explain what these tests test and why

```
Give an example
```

### And coding style tests

Explain what these tests test and why

```
Give an example
```

## Deployment

Add additional notes about how to deploy this on a live system

## Built With

* [Dropwizard](http://www.dropwizard.io/1.0.2/docs/) - The web framework used
* [Maven](https://maven.apache.org/) - Dependency Management
* [ROME](https://rometools.github.io/rome/) - Used to generate RSS Feeds

## Contributing

Please read [CONTRIBUTING.md](https://gist.github.com/PurpleBooth/b24679402957c63ec426) for details on our code of conduct, and the process for submitting pull requests to us.

## Versioning

We use [SemVer](http://semver.org/) for versioning. For the versions available, see the [tags on this repository](https://github.com/your/project/tags). 

## Authors

* **Billie Thompson** - *Initial work* - [PurpleBooth](https://github.com/PurpleBooth)

See also the list of [contributors](https://github.com/your/project/contributors) who participated in this project.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* Hat tip to anyone whose code was used
* Inspiration
* etc
