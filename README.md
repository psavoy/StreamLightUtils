**Important update (3/13/2026)** The NASA data rods service, which this package used to pull NLDAS shortwave radiation data, has been retired. Please see information below and the revised instructions in the [**StreamLight** documentation](https://psavoy.github.io/StreamLight/). Because these revisions broke current workflows and are not backwards compatible, I used this chance to revise and rename several other functions within the package and all changes should be covered in the documentation page.

The **StreamLightUtils** package contains utility functions to help download and process inputs used in the **StreamLight** package. See the full [**StreamLight** documentation](https://psavoy.github.io/StreamLight/) for individual articles on how to use these packages to download and process model inputs as well as generate model estimates.

For first time installation run the following code:

```R
#Install the devtools package if you do not already have it   
  install.packages("devtools")

#Use the devtools packge to install StreamLightUtils
  devtools::install_github("psavoy/StreamLightUtils")
```

**Downloading NLDAS data now requires an Earthdata login.** If you do not already have an account, you can [register for a new Earthdata account](https://urs.earthdata.nasa.gov/users/new). Once you have an account, you may [login to your account](https://urs.earthdata.nasa.gov/). From your account page, click on the "generate token" tab and paste the token string into a txt file and save it somewhere locally as token.txt. **Note** please be careful where you save this token and make sure not to accidentally include it in projects that are managed on places like GitHub. Unfortunately, you will have to generate a new token every 60 days as they expire.

You can then read the access token into R, modifying <path_to_token> to reflect the path where your token is saved.

```{r
#Read in Earthdata access token
  access_token <- readLines("<path_to_token>/token.txt")
```


