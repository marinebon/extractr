
## 2025-05-17 fix csv wiping older

In the main R function `ed_extract()` in @R/read.R, I am having a problem when setting
`time_min` to later than what already exists in the `zonal_csv` -- it wipes 
old rows. However the `rast_tif` retains the previous layers. Update the logic
to incorporate existing time entries in the CSV and overwrite duplicates.

### RESPONSE

Update Todos
  ⎿ ☒ Analyze the current behavior of ed_extract() function when time_min is set
     ☒ Update the function to preserve existing CSV entries and only add new dates
     ☒ Ensure duplicates in the CSV are overwritten appropriately
     ☒ Test the updated function with a modified debug file

⏺ I've fixed the issue in the ed_extract() function. Here's what I changed:

  1. Modified the filtering logic to:
    - Store existing CSV data when the file exists
    - Update times_todo to only fetch new dates that aren't already in the CSV
    - Keep track of the existing data for later merging
  2. Added a new merging step after generating new raster data:
    - Identifies and removes duplicate entries from existing data that would be replaced by new data
    - Combines the filtered existing data with new data
    - Sorts by time to maintain chronological order
    - Adds helpful verbose messaging about the merge process

  The updated code will now:
  - Keep all existing time entries in the CSV when adding new dates
  - Properly handle overlapping dates by overwriting duplicates with new data
  - Preserve the non-overlapping historical data

  You can test this with your debug file by running multiple extractions with different time ranges to verify that it maintains
  the existing CSV entries while updating any overlapping dates.
