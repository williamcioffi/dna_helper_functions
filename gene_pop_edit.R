fname <- ""

txt <- scan(fname, sep = "\n", what = "character", quiet = TRUE)

nlocus <- length(grep("Locus", txt))
nsamp <- length(txt) - nlocus - 1

locus_names <- txt[1:nlocus]
sampnames <- sapply(strsplit(txt[(nlocus + 1 + 1):length(txt)], " "), function(l) l[[1]][1])

m <- matrix(NA, nrow = nsamp, ncol = nlocus, dimnames = list(sampnames, locus_names))

for(i in 1:nsamp) {
	m[i, ] <- strsplit(txt[nlocus + 1 + i], " ")[[1]][4:(nlocus+3)]
}

m[m == "000000"] <- NA
mt <- t(m)

mt.complete <- mt[complete.cases(mt), ]
m.complete <- t(mt.complete)


nlocus.complete <- ncol(m.complete)

# # set.seed(1)
# ss <- sample(1:nlocus.complete, 200)

# m.complete.200 <- m.complete[, ss]
m.complete.200 <- m.complete

# generate the string
txt.new <- vector()
txt.new <- colnames(m.complete.200)
txt.new[length(txt.new) + 1] <- "Pop"
for(i in 1:nrow(m.complete.200)) {
	txt.new[length(txt.new) + 1] <- paste0(rownames(m.complete.200)[i], " , ", paste0(m.complete.200[i, ], collapse = " "))
}

# output the string to file \n between items
cat(txt.new, file = "out.gen", sep = '\n')