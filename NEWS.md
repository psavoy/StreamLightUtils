# StreamLightUtils 2.0.0

## 2.0.0 (3/13/2026)

- Update to version 2.0.0. This is a major change which is not backwards compatible with old workflows. The NASA data rods service, which this package used to pull NLDAS shortwave radiation data, has been retired. Please see information below and the revised instructions in the [**StreamLight** documentation](https://psavoy.github.io/StreamLight/). Because these revisions broke current workflows and are not backwards compatible, I used this chance to revise and rename several other functions within the package and all changes should be covered in the documentation page. Please reach out or start an issue if you encounter problems with this update. I am hoping to include a more elegant solution for accessing NLDAS data that does not require a token, but for not I wanted to implement a stop-gap measure so that users could continue to work in the interim.
