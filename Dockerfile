FROM reg.jalgos.ai/r-jalgos:4.0.2

WORKDIR /build
COPY ci /build/ci

RUN install2.r \
  --skipinstalled \
  --error \
  --deps TRUE \
  --repos https://cran.rstudio.com \
  RJSONIO

COPY . /build
