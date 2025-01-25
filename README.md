# Min-Sample-Size-Phenotypic-Dimorphism
Scripts and documentation for the article “Estimating Minimum Sample Size for Detecting Phenotypic Dimorphism Using Computational Simulations”<br>
VERSION: v.001.2024-01-22

## 1. Title of Dataset and Code:
Data and code from: Estimating Minimum Sample Size for Detecting Phenotypic Dimorphism Using Computational Simulations

## 2. Author Information
    Yibo Zhou          Nanjing University
    Xudong Hou         Nanjing University
    Yanhong Pan*       Nanjing University

## 3. Description of dataset and scripts
### Part 1. Code to generate simulation data and conduct ACR test
R version 4.2.3 (version in article)<br>
Platform: x86_64-pc-linux-gnu (64-bit)<br>

HPC(High-performance computing) Cluster blade server
A cluster comprises 8 compute nodes, so running 8 sets of experiments simultaneously is more time-efficient<br>
Each node employes 100 cores for parallel processing

`sourceCpp('./RcppArma_FUNC.cpp')`:
In `RcppArma_FUNC.cpp`, the functions `excessmassex`, `excessmassapp`, and several associated functions have been rewritten in C++.

### Part 2. Code to analyze and plot
R version 4.3.3 (version in article)<br>
R version 4.4.2 (version at 2025.01)<br>
Platform: x86_64-w64-mingw32/x64

### Part 3. Code to construct ANN model
R version 4.2.3 (version in article)<br>
Platform: x86_64-pc-linux-gnu (64-bit)<br>

### Part 4. Code to make prediction based on ANN model and data
R version 4.3.3 (version in article)<br>
R version 4.4.2 (version at 2025.01)<br>
Platform: x86_64-w64-mingw32/x64
