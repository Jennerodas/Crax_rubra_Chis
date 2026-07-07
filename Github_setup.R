#Github setup

install.packages("usethis")
library(usethis)

create_github_token()
gitcreds::gitcreds_set()

use_github()

use_readme_md()