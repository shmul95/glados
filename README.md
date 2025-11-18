<h1 align="center">
    GLaDOS
</h1>
<h2 align="center">
    Generic Language and Data Operand Syntax
</h2>

<div align="center">
  <img src="https://raw.githubusercontent.com/catppuccin/catppuccin/main/assets/palette/macchiato.png" width="600px"/>
  <p></p>
  <div align="center">
     <a href="https://github.com/shmul95/glados/stargazers">
        <img src="https://img.shields.io/github/stars/shmul95/glados?color=F5BDE6&labelColor=303446&style=for-the-badge&logo=starship&logoColor=F5BDE6">
     </a>
     <a href="https://github.com/shmul95/glados/">
        <img src="https://img.shields.io/github/repo-size/shmul95/glados?color=C6A0F6&labelColor=303446&style=for-the-badge&logo=github&logoColor=C6A0F6">
     </a>
     <a href="https://github.com/shmul95/glados/blob/main/LICENSE">
        <img src="https://img.shields.io/static/v1.svg?style=for-the-badge&label=License&message=BSD-3-Clause&colorA=313244&colorB=F5A97F&logo=unlicense&logoColor=F5A97F&"/>
     </a>
  </div>
  <br>
</div>

## Description

**GLaDOS** is an interpreter and a compiler developed entirely in **Haskell**.

> TODO: add language description

## Prerequisites

To compile and run **GLaDOS**, you must have the following tools installed on your system's PATH:

| Tool         | Role                                   | Requirement |
| :----------- | :------------------------------------- | :---------- |
| **Stack**    | Haskell project and dependency manager | _Required_  |
| **GNU Make** | Build automation tool.                 | _Required_  |
| **Git**      | For cloning the repository.            | _Required_  |

## Build

Clone the repository

```bash
git clone https://github.com/shmul95/glados
cd glados
```

Compile the project using `make`

> [!NOTE]
> stack will automatically install the correct version of GHC

```bash
make -j
```

## Use the compiler

```bash
./glados < file.scm
```

## License

The project is licensed under the BSD-3-Clause License. See [**LICENSE**](./LICENSE) for more details.
