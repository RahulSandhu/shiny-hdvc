<div align="justify">

# Thyroid Cancer Recurrence Dashboard

This repository contains the source code for an interactive Shiny dashboard
designed to help understand and visualize the factors related to the recurrence
of thyroid cancer in patients followed for at least 10 years. The project
utilizes a logistic regression model and provides a user-friendly interface for
data exploration.

## 🚀 Setup

The project is built using R and the Shiny framework. The following steps will
guide you through setting up the necessary environment and running the
dashboard.

1.  Clone the repository

    ```bash
    git clone https://github.com/RahulSandhu/shiny-hdvc
    cd your-repo-name
    ```

2.  Ensure R and renv are installed

    Make sure you have R installed on your system. This project uses `renv` for
    dependency management to ensure reproducibility.

    ```r
    install.packages("renv")
    ```

3.  Restore the project environment

    The project uses a local `.Rprofile` and `renv.lock` file to manage
    dependencies. Run the following R command in the project directory to
    install all necessary packages:

    ```r
    renv::restore()
    ```

4.  Run the application

    Once the environment is restored, you can launch the Shiny application:

    ```r
    shiny::runApp("src/app")
    ```

## 🎓 Acknowledgments

- Borzooei, S. & Tarokhian, A. (2023). **Differentiated Thyroid Cancer
  Recurrence** [Dataset]. UCI Machine Learning Repository.
  *https://doi.org/10.24432/C5632J.*
- Developed as part of the Health Data Science Master’s program at Universitat
  Rovira i Virgili (URV)

</div>
