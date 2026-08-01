<h1 align="center" id="title">java2pyi</h1>

<p align="center"><img src="https://socialify.git.ci/n08i40k/java2pyi/image?description=1&amp;font=JetBrains+Mono&amp;language=1&amp;name=1&amp;owner=1&amp;pattern=Plus&amp;stargazers=1&amp;theme=Auto" alt="project-image"></p>

<p id="description">Java‑to‑PYI is a Rust tool that reads compiled Java classes from <code>.jar</code> archives and generates Python stub files (.pyi) with type definitions only. It was originally designed to generate class declarations to provide accurate type hints in the Telegram codebase when developing Exteragram plugins.</p>

## Installation from `crates.io`

```
cargo install java2pyi
```

## Building from source code and installation steps

1. Clone this repo

```
git clone https://github.com/n08i40k/java2pyi
```

2. Compile and install

```
cargo install --path .
```

## Usage example

1. Collect the `.jar` files you want stubs for, including the JDK ones (required for all use-cases, otherwise non-primitive java types won't be resolved)

```
mkdir jars
cp "$JAVA_HOME/jmods/../lib/../jre/lib/rt.jar" ./jars/ # or any jar holding the JDK classes
cp ./Telegram/TMessagesProj/build/outputs/*.jar ./jars/
```

2. Create directory for stub files

```
mkdir stubs
```

3. Start generator

```
java2pyi -i ./jars -o ./stubs
```

Where `-i` is a `.jar` file or a directory searched recursively for `.jar` files, and `-o` is the output directory for generated stubs.
