# Tesis_Individuo
Data is organised in several folders.

01_raw_data: contains the initial data directly from the field or the database.

02_clean_data: contains the cleaned data, obtained from the raw database. It is important to obtain clean data when possible through the scripts, to be sure that all error and id generation is updated.

03_scripts: contains all R scripts used for every needed process. They have been named as: 01_: from raw_field to raw_joined (i.e. join data and include the names). 02_: from raw_joined to clean data (i.e. including unique_ids, check ids with other databases, check NAs and key calculus). 03_: scripts to perform basic calculus and export information from clean data to specific projects. 04_: join with external databases (e.g. Landsat, climate, soils, etc.).

  Within each field the order of the clean data will be:
  00: climate
  01: plot
  02: target

04_documents: we included (1) metadata: of clean data variables names, data type, units and description

05_outputs: figures or combined tables with outputs.
