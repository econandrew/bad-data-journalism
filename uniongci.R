setwd('~/Documents/Ideas/Blog/538-Unions/analysis')

# Load data
gci <- read.csv('data/gci.csv', stringsAsFactors=F)
union <- read.csv('data/union.csv', stringsAsFactors=F, na.strings="..")
gdp <- read.csv('data/gdp.csv', stringsAsFactors=F)
irreligion <- read.csv('data/irreligion.csv', stringsAsFactors=F)

# Clean data
union$CountryCode <- sub(":.*", "", union$Country)
gdp$CountryCode <- sub(":.*", "", gdp$Country)

# Merge data
ugci <- data.frame(Country = gci$Entity, CountryCode = gci$Entity.code, GCI2010 = gci$Value)
ugci <- merge(ugci, data.frame(CountryCode = union$CountryCode, Union2010 = union$X2010), by = "CountryCode")
ugci <- merge(ugci, irreligion, by = "CountryCode")
ugci <- merge(ugci, data.frame(CountryCode = gdp$CountryCode, GDP2010 = gdp$X2010, GDP2012 = gdp$X2012))


############################################################################ ###
# Problem 1: Plot Working-Hotelling confidence band for regression line     ####
############################################################################ ###

fit <- lm(GCI2010 ~ Union2010, data = ugci)

pbounds <- data.frame(Union2010 = -10:100)
CI <- predict(fit, pbounds, se.fit=T)
W <- sqrt( 2 * qf(0.90, 2, nrow(ugci)-2) ) # working-hotelling
bands <- cbind( CI$fit - W * CI$se.fit, CI$fit + W * CI$se.fit )

png('out/uniongci-bands.png',width=1000, height=1000, res=180)
plot(GCI2010 ~ Union2010, data=ugci,
     pch=19, xlim=c(0, 80), ylim=c(0,6), frame.plot=F, cex=0.75,
     xlab="% of workers in a union", ylab="Global competitiveness score",
     main="Unionization and economic competitiveness (II)")
abline( fit, col="red" )
polygon(c(pbounds$Union2010,rev(pbounds$Union2010)), c(bands[,1],rev(bands[,2])), lty=0, col=rgb(1,0,0,0.2))
r2 <- formatC(summary(fit)$r.squared, format="f", digits = 2)
p <- formatC(anova(fit)$'Pr(>F)'[1], format="f",  digits = 2)
text(70, 1, bquote(.('p-value') ==.(p)))
text(70, 0.5, bquote(R^2 == .(r2)))
dev.off()

############################################################################ ###
# Problem 2: Bivariate analysis using irreligion                            ####
############################################################################ ###

fit.irreligion <- lm(GCI2010 ~ Irreligion, data = ugci)

png('out/irreligiongci.png',width=1000, height=1000, res=180)
plot(GCI2010 ~ Irreligion, data=ugci,
     pch=19, xlim=c(20, 100), ylim=c(0,6), frame.plot=F, cex=0.75,
     xlab="% of irreligious citizens", ylab="Global competitiveness score",
     main="Irreligion and economic competitiveness")
abline( fit.irreligion, col="red" )
r2 <- formatC(summary(fit.irreligion)$r.squared, format="f", digits = 2)
p <- formatC(anova(fit.irreligion)$'Pr(>F)'[1], format="f",  digits = 5)
text(90, 1, bquote(.('p-value') ==.(p)))
text(90, 0.5, bquote(R^2 == .(r2)))
dev.off()

############################################################################ ###
# Problem 3: Influence of Nordic countries                                  ####
############################################################################ ###

nordics = c('Denmark', 'Norway', 'Finland', 'Sweden')
fit.nonnordic <- lm(GCI2010 ~ Union2010, data = ugci[!(ugci$Country %in% nordics),])

png('out/uniongci-nonnordic.png',width=1000, height=1000, res=180)
plot(GCI2010 ~ Union2010, data=ugci,
     pch=19, xlim=c(0, 80), ylim=c(0,6), frame.plot=F, cex=0.75,
     xlab="% of workers in a union", ylab="Global competitiveness score",
     main="Unionization and economic competitiveness (III)")
points(GCI2010 ~ Union2010, data=ugci[(ugci$Country %in% nordics),],
       pch=1, cex=1.8, col="blue")
abline( fit.nonnordic, col="red" )
dev.off()

############################################################################ ###
# Problem 4: Different dependent variable                                   ####
############################################################################ ###

ugci$growth1012 <- (sqrt(ugci$GDP2012/ugci$GDP2010)-1)*100
fit.gdpgrowth <- lm(growth1012 ~ Union2010, data = ugci)

png('out/uniongdpgrowth.png',width=1000, height=1000, res=180)
plot(growth1012 ~ Union2010, data=ugci,
     pch=19, xlim=c(0, 80), ylim=c(-10,10), frame.plot=F, cex=0.75,
     xlab="% of workers in a union", ylab="Annualised growth GDP per capita (constant PPP), 2010-2012",
     main="Unionization and actual economic growth")
abline( fit.gdpgrowth, col="red", lty="longdash" )
dev.off()
