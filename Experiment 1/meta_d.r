trials2counts <- function(stimID, response, rating, nRatings, padAmount = 0, padCells = 0) {
    nR_S1 <- list()
    nR_S2 <- list()

    if (padAmount == 0) {
        padAmount <- 1 / (2 * nRatings)
    }
    # S1 responses
    for (r in nRatings:1) {
        cs1 <- 0
        cs2 <- 0
        for (i in 1:length(stimID)) {
            s <- stimID[i]
            x <- response[i]
            y <- rating[i]

            if ((s == 0) & (x == 0) & (y == r)) {
                (cs1 <- cs1 + 1)
            }
            if ((s == 1) & (x == 0) & (y == r)) {
                (cs2 <- cs2 + 1)
            }
        }
        nR_S1 <- append(nR_S1, cs1)
        nR_S2 <- append(nR_S2, cs2)
    }

    # S2 responses
    for (r in 1:nRatings) {
        cs1 <- 0
        cs2 <- 0
        for (i in 1:length(stimID)) {
            s <- stimID[i]
            x <- response[i]
            y <- rating[i]

            if ((s == 0) & (x == 1) & (y == r)) {
                (cs1 <- cs1 + 1)
            }
            if ((s == 1) & (x == 1) & (y == r)) {
                (cs2 <- cs2 + 1)
            }
        }
        nR_S1 <- append(nR_S1, cs1)
        nR_S2 <- append(nR_S2, cs2)
    }


    # pad response counts to avoid zeros
    nR_S1 <- as.numeric(nR_S1)
    nR_S2 <- as.numeric(nR_S2)
    if (padCells == 1) {
        nR_S1 <- lapply(nR_S1, FUN = function(x) x + padAmount)
        nR_S2 <- lapply(nR_S2, FUN = function(x) x + padAmount)
    }

    # Combine into lists
    newlist <- list(nR_S1, nR_S2)
}

data <- read.csv("Data/Sorted participant files/Experiment 1/A1.csv")
data <- data %>%
    mutate(ResponseType = ifelse(ResponseType == "Old", 0, 1)) %>%
    mutate(CorrResponse = ifelse(CorrResponse == "Old", 0, 1))

data <- data %>%
    filter(Phase == "Test") %>%
    filter(Trial != "CJ") %>%
    filter(!is.na(Judgment))

data1 <- data %>%
    select(CorrResponse)

stimID <- data1$CorrResponse

data2 <- data %>%
    select(ResponseType)

response <- data2$ResponseType

data3 <- data %>%
    select(Judgment)

rating <- as.numeric(data3$Judgment)
rating <- rating

nRatings <- 100

newlist <- trials2counts(stimID, response, rating, nRatings)


# Type 1 counts
N <- sum(counts[1:(nratings*2)])
S <- sum(counts[(nratings*2+1):(nratings*4)])
H <- sum(counts[(nratings*3+1):(nratings*4)])
M <- sum(counts[(nratings*2+1):(nratings*3)])
FA <- sum(counts[(nratings+1):(nratings*2)])
CR <- sum(counts[1:(nratings)])


## TYPE 1 SDT BINOMIAL MODEL
H ~ dbin(h,S)
FA ~ dbin(f,N)
h <- phi(d1/2-c1)
f <- phi(-d1/2-c1)

# Type 1 priors
c1 ~ dnorm(0, 2)
d1 ~ dnorm(0, 0.5)

## TYPE 2 SDT MODEL (META-D)
# Multinomial likelihood for response counts ordered as c(nR_S1,nR_S2)
counts[1:(nratings)] ~ dmulti(prT[1:(nratings)],CR)
counts[(nratings+1):(nratings*2)] ~ dmulti(prT[(nratings+1):(nratings*2)],FA)
counts[(nratings*2+1):(nratings*3)] ~ dmulti(prT[(nratings*2+1):(nratings*3)],M)
counts[(nratings*3+1):(nratings*4)] ~ dmulti(prT[(nratings*3+1):(nratings*4)],H)

# Means of SDT distributions
S2mu <- meta_d/2
S1mu <- -meta_d/2

# Calculate normalisation constants
C_area_rS1 <- phi(c1 - S1mu)
I_area_rS1 <- phi(c1 - S2mu)
C_area_rS2 <- 1-phi(c1 - S2mu)
I_area_rS2 <- 1-phi(c1 - S1mu)

# Get nC_rS1 probs
pr[1] <- phi(cS1[1] - S1mu)/C_area_rS1  
for (k in 1:(nratings-2)) {                
  pr[k+1] <- (phi(cS1[k+1] - S1mu)-phi(cS1[k] - S1mu))/C_area_rS1
}
pr[(nratings)] <- (phi(c1 - S1mu)-phi(cS1[(nratings-1)] - S1mu))/C_area_rS1   

# Get nI_rS2 probs
pr[(nratings+1)] <- ((1-phi(c1 - S1mu))-(1-phi(cS2[1] - S1mu)))/I_area_rS2 
for (k in 1:(nratings-2)) {                
  pr[nratings+1+k] <- ((1-phi(cS2[k] - S1mu))-(1-phi(cS2[k+1] - S1mu)))/I_area_rS2
}
pr[(nratings*2)] <- (1-phi(cS2[(nratings-1)] - S1mu))/I_area_rS2

# Get nI_rS1 probs
pr[(nratings*2+1)] <- phi(cS1[1] - S2mu)/I_area_rS1
for (k in 1:(nratings-2)) {
  pr[(nratings*2+1+k)] <- (phi(cS1[k+1] - S2mu)-phi(cS1[k] - S2mu))/I_area_rS1 
}
pr[(nratings*3)] <- (phi(c1 - S2mu)-phi(cS1[(nratings-1)] - S2mu))/I_area_rS1  

# Get nC_rS2 probs
pr[(nratings*3+1)] <- ((1-phi(c1 - S2mu))-(1-phi(cS2[1] - S2mu)))/C_area_rS2 
for (k in 1:(nratings-2)) {                
  pr[(nratings*3+1+k)] <- ((1-phi(cS2[k] - S2mu))-(1-phi(cS2[k+1] - S2mu)))/C_area_rS2
}
pr[(nratings*4)] <- (1-phi(cS2[(nratings-1)] - S2mu))/C_area_rS2

# Avoid underflow of probabilities
for (i in 1:(nratings*4)) {
  prT[i] <- ifelse(pr[i] < Tol, Tol, pr[i])
}

# Specify ordered prior on criteria (bounded above and below by Type 1 c1) 
for (j in 1:(nratings-1)) {
  cS1_raw[j] ~ dnorm(0,2) I(,c1-Tol)
  cS2_raw[j] ~ dnorm(0,2) I(c1+Tol,)
}
cS1[1:(nratings-1)] <- sort(cS1_raw)
cS2[1:(nratings-1)] <- sort(cS2_raw)

# Type 2 priors
meta_d ~ dnorm(d1,0.5)
