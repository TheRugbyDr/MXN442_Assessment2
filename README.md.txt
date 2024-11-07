# MXN442 Assignment 2- Project
## Description
Welcome to the GIT Hub set up to host the code, dataset, and report that collectively contribute to my submission of assignment 2 for MXN442 (Modern Statistical Computing Techniques) hosted by QUT (Queensland University of Technology). In this repo you will find: 1x R Script, 2x child folders containing 64 csv/xlsx files, 1x PDF, and this README.md.


## Installation
There are package dependencies which will need to be installed but an automatic installation has been coded into the script. The exception is the keras package which will require a python environment and tensorflow to be installed: see <https://www.tensorflow.org/guide> if the r script does not install this for you automatically. I have also included below some troubleshooting steps I had to work out to solve this same issue. Note this will be very specific to my circumstances:

### TensorFlow installation
1. Install Git on Your System
You need to install Git on your Windows system. Here’s how:

a. Download Git for Windows:
- Visit the official Git website: https://git-scm.com/download/win
- Click the link to download the latest version of Git for Windows.

b. Run the Installer:
- Locate the downloaded installer (`Git-<version>-64-bit.exe`) and double-click it to start the installation process.

c. Follow the Installation Steps:
i. **Welcome Screen:** Click **Next**.
ii. **Select Destination Location:** Leave the default installation path or choose a different one. Click **Next**.
iii. **Select Components:** Leave the default selections. Click **Next**.
iv. **Select Start Menu Folder:** Leave the default. Click **Next**.
v. **Choosing the Default Editor Used by Git:** Select your preferred text editor (e.g., Visual Studio Code). Click **Next**.
vi. **Adjusting Your PATH Environment:** Select **"Git from the command line and also from 3rd-party software"** to ensure Git is added to your system’s PATH. Click **Next**.
vii. **Choosing HTTPS Transport Backend:** Leave the default selection ("Use the OpenSSL library"). Click **Next**.
vii. **Configuring the Line Ending Conversions:** Leave the default selection ("Checkout Windows-style, commit Unix-style line endings"). Click **Next**.
ix. **Configuring the Terminal Emulator:** Leave the default selection ("Use MinTTY (the default terminal of MSYS2)"). Click **Next**.
x. **Choose the Default Behavior of Git Pull:** Leave the default selection. Click **Next**.
xi. **Choose Credential Helper:** Leave the default selection. Click **Next**.
xii. **Configuring Extra Options:** Leave the defaults or adjust as preferred. Click **Next**.
xii. **Configuring Experimental Options:** Leave unchecked unless specific features are needed. Click **Install**.

d. Complete the Installation:
- Wait for the installation to complete and click **Finish** when done.

2. Verify Git Installation and PATH Configuration
To ensure Git is installed and available on your PATH:

a. Open Command Prompt or PowerShell:
- Press `Win + R`, type `cmd`, and press Enter, or search for “Command Prompt” or “PowerShell” in the Start menu.

b. Check Git Version:
Run the following command:

```bash
git --version
```

- If Git is correctly installed, this should display the installed Git version.
- If you see an error like `'git' is not recognized`, Git is not on your PATH.

3. Add Git to Your PATH Manually (If Necessary)
If Git is not on your PATH, you can add it manually:

a. Locate Git Installation Directory:
- By default, Git is installed in `C:\Program Files\Git\bin`.

b. Add Git to System PATH:
i. **Open System Properties:** Press `Win + R`, type `sysdm.cpl`, and press Enter. Go to the **Advanced** tab and click **Environment Variables**.
ii. **Edit System Variables:** Under **System Variables**, find the `Path` variable. Select it and click **Edit**.
iii. **Add Git Path:** Click **New**, enter `C:\Program Files\Git\bin`, and click **OK** to close all windows.

**Restart RStudio and Command Prompt** to ensure the updated PATH is recognized.

4. Retry the Python Installation in R
With Git installed and on your PATH, retry the `install_python()` command in R.

a. Restart R or RStudio:
- This ensures R recognizes the updated PATH environment variable.

b. Run the Installation Command:

```r
library(reticulate)
install_python(version = "3.8.10")
```

5. Proceed with TensorFlow Installation
After successfully installing Python, proceed with TensorFlow:

```r
library(tensorflow)
install_tensorflow(version = "2.13.1")
```

6. Verify the Installation

a. Check Python Configuration:

```r
library(reticulate)
py_config()
```

- Confirm that Python 3.8.10 is listed, and the path points to the Python installation managed by `reticulate`.

b. Test TensorFlow:

```r
library(tensorflow)
tf$constant("Hello, TensorFlow!")
```
If TensorFlow is properly installed, this should output a TensorFlow tensor.

## Usage
This was an academic exercise to attempt to replicate the algorithm published by Ziel, F (DOI: 10.1109/OAJPE.2022.3160933). It has no endorsement for use in a live forecasting situation


## Dataset Information
All datasets were downloaded from <https://ieee-dataport.org/competitions/day-ahead-electricity-demand-forecasting-post-covid-paradigm>. There is limited information on the origin of the data and beyond a hyperlink to email the author, little support. A benchmark code was provided but it can be considered a naïve forecasting model.


## Contact Information
For questions or feedback, you're welcome to contact [James Donovan] at [n9164090@qut.edu.au]. Please note a reply is unlikely for I'll have graduated :)))
