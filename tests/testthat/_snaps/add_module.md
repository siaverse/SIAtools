# adding module to module package is silent about DESCRIPTION

    Code
      add_module(name = "test")
    Message
      x There is no SIA Modules Manifest file at 'inst/sia/modules.yml'
      i Creating a new one.
      > 'inst/sia/modules.yml' YAML file with the SIA Modules Manifest has been created. Please inspect and edit.
      > 'R/sm_test.R' file with your new module has been created. You can write your code here.
      > Edit the title of your module in the YAML.
      > Chose a category from the options listed in the YAML.

# subsequent adding of modules in ordinary package produces expected messages

    Code
      add_module(name = "test1")
    Message
      i Adding Config/ShinyItemAnalysis/module: true to 'DESCRIPTION'.
      x There is no SIA Modules Manifest file at 'inst/sia/modules.yml'
      i Creating a new one.
      > 'inst/sia/modules.yml' YAML file with the SIA Modules Manifest has been created. Please inspect and edit.
      > 'R/sm_test1.R' file with your new module has been created. You can write your code here.
      > Edit the title of your module in the YAML.
      > Chose a category from the options listed in the YAML.

---

    Code
      add_module(name = "test2")
    Message
      > 'inst/sia/modules.yml' YAML file with the SIA Modules Manifest has been created. Please inspect and edit.
      > 'R/sm_test2.R' file with your new module has been created. You can write your code here.
      > Edit the title of your module in the YAML.
      > Chose a category from the options listed in the YAML.

# subsequent adding of modules in module package produces expected messages

    Code
      add_module(name = "test1")
    Message
      x There is no SIA Modules Manifest file at 'inst/sia/modules.yml'
      i Creating a new one.
      > 'inst/sia/modules.yml' YAML file with the SIA Modules Manifest has been created. Please inspect and edit.
      > 'R/sm_test1.R' file with your new module has been created. You can write your code here.
      > Edit the title of your module in the YAML.
      > Chose a category from the options listed in the YAML.

---

    Code
      add_module(name = "test2")
    Message
      > 'inst/sia/modules.yml' YAML file with the SIA Modules Manifest has been created. Please inspect and edit.
      > 'R/sm_test2.R' file with your new module has been created. You can write your code here.
      > Edit the title of your module in the YAML.
      > Chose a category from the options listed in the YAML.

# adding a pre-specified module is silent about title and category editing

    Code
      add_module(name = "category_given", category = "Modules")
    Message
      x There is no SIA Modules Manifest file at 'inst/sia/modules.yml'
      i Creating a new one.
      > 'inst/sia/modules.yml' YAML file with the SIA Modules Manifest has been created. Please inspect and edit.
      > 'R/sm_category_given.R' file with your new module has been created. You can write your code here.
      > Edit the title of your module in the YAML.

---

    Code
      add_module(name = "title_given", title = "Test module")
    Message
      > 'inst/sia/modules.yml' YAML file with the SIA Modules Manifest has been created. Please inspect and edit.
      > 'R/sm_title_given.R' file with your new module has been created. You can write your code here.
      > Chose a category from the options listed in the YAML.

---

    Code
      add_module(name = "both_given", title = "Test module", category = "Modules")
    Message
      > 'inst/sia/modules.yml' YAML file with the SIA Modules Manifest has been created. Please inspect and edit.
      > 'R/sm_both_given.R' file with your new module has been created. You can write your code here.

