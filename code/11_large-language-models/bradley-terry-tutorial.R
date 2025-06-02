library(tidyverse)
library(BradleyTerry2)

# going to drop the ties from the ice hockey dataset;
# they're informative in principle, but the current
# version of the package can't handle them
d <- BradleyTerry2::icehockey |>
  filter(result != 0.5)

mod <- BTm(outcome = result,
           player1 = visitor,
           player2 = opponent,
           data = d)

mod$coefficients
sort(mod$coefficients)
