# Haskell Web Audio

Web Audio API dsl in Haskell

## Autores

- [Gonzalo Arca](https://github.com/gonzaloarca)
- [Camila Borinsky](https://github.com/camilaborinsky)

## Ejecución

1. Clonar el proyecto
2. Instalar dependencias

```
stack build
```

3. En el archivo `Main.hs` describir el sistema de audio utilizando las primitivas del lenguaje.
4. Elegir la interpretación monádica especificando el tipo de la expresión definida en el punto anterior en el `Main.hs`.
5. Especificar en el `Main.hs` el path del archivo de salida.
6. Ejecutar el programa

```
stack run
```

## Primitivas del lenguaje

Las posibles sentencias que se pueden utilizar para describir el sistema de audio son las definidas en el archivo `src/WebAudio/WebAudioMonad.hs`
Los tipos que se pueden utilizar y a los que se hace referencia en la clase `WebAudioMonad` pueden encontrarse en el archivo `src/WebAudio/Types.hs`
