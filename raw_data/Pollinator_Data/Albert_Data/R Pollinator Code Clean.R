library(readxl)
Pollinator_Data <- read_excel("Pollinator Data.xlsx")
View(Pollinator_Data)
attach(Pollinator_Data)
Ave_Pollinator_New <- read_excel("Ave_Pollinator_Data.xlsx")
attach(Ave_Pollinator_New)
View(Ave_Pollinator_New)

Ave_Pollinator_Data <- aggregate(Pollinator_Data, list(Pollinator_Data$`Site#`), mean)
Urban_Rural <- aggregate(Ave_Pollinator_New, list(Ave_Pollinator_New$Type))

with(Ave_Pollinator_Data, boxplot(`abundance all sp.`, small_pollinators, medium_pollinators, large_pollinators, `abundance of bees`, small_bees, medium_bees, large_bees))
with(Ave_Pollinator_Data, boxplot(`diversity all groups (bee families + other spp.)`, `diversity bee families`))

##make scatter plots of ave data
with(Ave_Pollinator_Data, scatter.smooth(`DistanceFromCityCenter (KM)`, `diversity all groups (bee families + other spp.)`))
with(Ave_Pollinator_Data, scatter.smooth(`DistanceFromCityCenter (KM)`, `diversity bee families`))

with(Ave_Pollinator_Data, scatter.smooth(`DistanceFromCityCenter (KM)`, `abundance all sp.`))
with(Ave_Pollinator_Data, scatter.smooth(`DistanceFromCityCenter (KM)`, small_pollinators))
with(Ave_Pollinator_Data, scatter.smooth(`DistanceFromCityCenter (KM)`, medium_pollinators))
with(Ave_Pollinator_Data, scatter.smooth(`DistanceFromCityCenter (KM)`, large_pollinators))

with(Ave_Pollinator_Data, scatter.smooth(`DistanceFromCityCenter (KM)`, `abundance of bees`))
with(Ave_Pollinator_Data, scatter.smooth(`DistanceFromCityCenter (KM)`, small_bees))
with(Ave_Pollinator_Data, scatter.smooth(`DistanceFromCityCenter (KM)`, medium_bees))
with(Ave_Pollinator_Data, scatter.smooth(`DistanceFromCityCenter (KM)`, large_bees))

with(Ave_Pollinator_Data, scatter.smooth(`DistanceFromCityCenter (KM)`, `sweat bees (Halictidae sp.)`))
with(Ave_Pollinator_Data, scatter.smooth(`DistanceFromCityCenter (KM)`, `leaf cutter bees (Megachile sp.)`))
with(Ave_Pollinator_Data, scatter.smooth(`DistanceFromCityCenter (KM)`, `orange beetle (Rhagoycha fulva)`))
with(Ave_Pollinator_Data, scatter.smooth(`DistanceFromCityCenter (KM)`, `Apis mellifera (Large)`))

##check normality
with(Ave_Pollinator_Data, qqnorm(`abundance all sp.`))
with(Ave_Pollinator_Data, qqnorm(`abundance of bees`))

with(Ave_Pollinator_Data, qqnorm(`diversity all groups (bee families + other spp.)`))
with(Ave_Pollinator_Data, qqnorm(`diversity bee families`))

with(Ave_Pollinator_Data, qqnorm(small_bees))
with(Ave_Pollinator_Data, qqnorm(medium_bees))
with(Ave_Pollinator_Data, qqnorm(large_bees))

with(Ave_Pollinator_Data, qqnorm(small_pollinators))
with(Ave_Pollinator_Data, qqnorm(medium_pollinators))
with(Ave_Pollinator_Data, qqnorm(large_pollinators))

with(Ave_Pollinator_Data, qqnorm(`sweat bees (Halictidae sp.)`))
with(Ave_Pollinator_Data, qqnorm(`leaf cutter bees (Megachile sp.)`))
with(Ave_Pollinator_Data, qqnorm(`orange beetle (Rhagoycha fulva)`))
with(Ave_Pollinator_Data, qqnorm(`Apis mellifera (Large)`))

##sqrt transformation
sqrt_pollinator_abundance <- with(Ave_Pollinator_Data, sqrt(`abundance all sp.`))
boxplot(sqrt_pollinator_abundance)
qqnorm(sqrt_pollinator_abundance)
with(Ave_Pollinator_Data, scatter.smooth(`DistanceFromCityCenter (KM)`, sqrt_pollinator_abundance))

sqrt_bee_abundance <- with(Ave_Pollinator_Data, sqrt(`abundance of bees`))
boxplot(`abundance of bees`)
qqnorm(sqrt_bee_abundance)
with(Ave_Pollinator_Data, scatter.smooth(`DistanceFromCityCenter (KM)`, sqrt_bee_abundance))

sqrt_small_bees <- with(Ave_Pollinator_Data, sqrt(small_bees))
boxplot(small_bees)
qqnorm(sqrt_small_bees)
with(Ave_Pollinator_Data, scatter.smooth(`DistanceFromCityCenter (KM)`, sqrt_small_bees))

sqrt_medium_bees <- with(Ave_Pollinator_Data, sqrt(medium_bees))
boxplot(medium_bees)
qqnorm(sqrt_medium_bees)
with(Ave_Pollinator_Data, scatter.smooth(`DistanceFromCityCenter (KM)`, sqrt_medium_bees))

sqrt_large_bees <- with(Ave_Pollinator_Data, sqrt(large_bees))
boxplot(large_bees)
qqnorm(sqrt_large_bees)
with(Ave_Pollinator_Data, scatter.smooth(`DistanceFromCityCenter (KM)`, sqrt_large_bees))

sqrt_small_pollinators <- with(Ave_Pollinator_Data, sqrt(small_pollinators))
boxplot(small_pollinators)
qqnorm(sqrt_small_pollinators)
with(Ave_Pollinator_Data, scatter.smooth(`DistanceFromCityCenter (KM)`, sqrt_small_pollinators))

sqrt_medium_pollinators <- with(Ave_Pollinator_Data, sqrt(medium_pollinators))
boxplot(medium_pollinators)
qqnorm(sqrt_medium_pollinators)
with(Ave_Pollinator_Data, scatter.smooth(`DistanceFromCityCenter (KM)`, sqrt_medium_pollinators))

sqrt_large_pollinators <- with(Ave_Pollinator_Data, sqrt(large_pollinators))
boxplot(large_pollinators)
qqnorm(sqrt_large_pollinators)
with(Ave_Pollinator_Data, scatter.smooth(`DistanceFromCityCenter (KM)`, sqrt_large_pollinators))

sqrt_sweat_bees <- with(Ave_Pollinator_Data, sqrt(`sweat bees (Halictidae sp.)`))
boxplot(sqrt_sweat_bees)
qqnorm(sqrt_sweat_bees)
with(Ave_Pollinator_Data, scatter.smooth(`DistanceFromCityCenter (KM)`, sqrt_sweat_bees))

sqrt_leafcutter_bees <- with(Ave_Pollinator_Data, sqrt(`leaf cutter bees (Megachile sp.)`))
boxplot(sqrt_leafcutter_bees)
qqnorm(sqrt_leafcutter_bees)
with(Ave_Pollinator_Data, scatter.smooth(`DistanceFromCityCenter (KM)`, sqrt_leafcutter_bees))

sqrt_orange_beetle <- with(Ave_Pollinator_Data, sqrt(`orange beetle (Rhagoycha fulva)`))
boxplot(sqrt_orange_beetle)
qqnorm(sqrt_orange_beetle)
with(Ave_Pollinator_Data, scatter.smooth(`DistanceFromCityCenter (KM)`, sqrt_orange_beetle))

sqrt_apis_melifera <- with(Ave_Pollinator_Data, sqrt(`Apis mellifera (Large)`))
boxplot(sqrt_apis_melifera)
qqnorm(sqrt_apis_melifera)
with(Ave_Pollinator_Data, scatter.smooth(`DistanceFromCityCenter (KM)`, sqrt_apis_melifera))

sqrt_diversity_all <- with(Ave_Pollinator_Data, sqrt(`diversity all groups (bee families + other spp.)`))
boxplot(sqrt_diversity_all)
qqnorm(sqrt_diversity_all)
with(Ave_Pollinator_Data, scatter.smooth(`DistanceFromCityCenter (KM)`, sqrt_diversity_all))

sqrt_diversity_bees <- with(Ave_Pollinator_Data, sqrt(`diversity bee families`))
boxplot(sqrt_diversity_bees)
qqnorm(sqrt_diversity_bees)
with(Ave_Pollinator_Data, scatter.smooth(`DistanceFromCityCenter (KM)`, sqrt_diversity_bees))

##histograms
with(Ave_Pollinator_Data, hist(`abundance all sp.`))
hist(sqrt_pollinator_abundance) ##better but not great

with(Ave_Pollinator_Data, hist(`abundance of bees`))
hist(sqrt_bee_abundance) ##better but not great

with(Ave_Pollinator_Data, hist(`diversity all groups (bee families + other spp.)`))
hist(sqrt_diversity_all)

with(Ave_Pollinator_Data, hist(`diversity bee families`))
hist(sqrt_diversity_bees)

with(Ave_Pollinator_Data, hist(small_pollinators))
hist(sqrt_small_pollinators)

with(Ave_Pollinator_Data, hist(medium_pollinators))
hist(sqrt_medium_pollinators)

with(Ave_Pollinator_Data, hist(large_pollinators))
hist(sqrt_large_pollinators)

with(Ave_Pollinator_Data, hist(small_bees))
hist(sqrt_small_bees)

with(Ave_Pollinator_Data, hist(medium_bees))
hist(sqrt_medium_bees)

with(Ave_Pollinator_Data, hist(large_bees))
hist(sqrt_large_bees)

with(Ave_Pollinator_Data, hist(`sweat bees (Halictidae sp.)`))
hist(sqrt_sweat_bees)

with(Ave_Pollinator_Data, hist(`leaf cutter bees (Megachile sp.)`))
hist(sqrt_leafcutter_bees)

with(Ave_Pollinator_Data, hist(`orange beetle (Rhagoycha fulva)`))
hist(sqrt_orange_beetle)

with(Ave_Pollinator_Data, hist(`Apis mellifera (Large)`))
hist(sqrt_apis_melifera)

##pearson correlation test
with(Ave_Pollinator_Data, cor.test(`DistanceFromCityCenter (KM)`, `abundance all sp.`))
with(Ave_Pollinator_Data, cor.test(`DistanceFromCityCenter (KM)`, `abundance of bees`))

with(Ave_Pollinator_Data, cor.test(`DistanceFromCityCenter (KM)`, small_pollinators))
with(Ave_Pollinator_Data, cor.test(`DistanceFromCityCenter (KM)`, medium_pollinators))
with(Ave_Pollinator_Data, cor.test(`DistanceFromCityCenter (KM)`, large_pollinators))

with(Ave_Pollinator_Data, cor.test(`DistanceFromCityCenter (KM)`, sqrt_small_pollinators))
with(Ave_Pollinator_Data, cor.test(`DistanceFromCityCenter (KM)`, sqrt_medium_pollinators))
with(Ave_Pollinator_Data, cor.test(`DistanceFromCityCenter (KM)`, sqrt_large_pollinators))

with(Ave_Pollinator_Data, cor.test(`DistanceFromCityCenter (KM)`, small_bees))
with(Ave_Pollinator_Data, cor.test(`DistanceFromCityCenter (KM)`, medium_bees))
with(Ave_Pollinator_Data, cor.test(`DistanceFromCityCenter (KM)`, large_bees))

with(Ave_Pollinator_Data, cor.test(`DistanceFromCityCenter (KM)`, sqrt_small_bees))
with(Ave_Pollinator_Data, cor.test(`DistanceFromCityCenter (KM)`, sqrt_medium_bees))
with(Ave_Pollinator_Data, cor.test(`DistanceFromCityCenter (KM)`, sqrt_large_bees))

with(Ave_Pollinator_Data, cor.test(`DistanceFromCityCenter (KM)`, `sweat bees (Halictidae sp.)`))
with(Ave_Pollinator_Data, cor.test(`DistanceFromCityCenter (KM)`, `leaf cutter bees (Megachile sp.)`))
with(Ave_Pollinator_Data, cor.test(`DistanceFromCityCenter (KM)`, `orange beetle (Rhagoycha fulva)`))
with(Ave_Pollinator_Data, cor.test(`DistanceFromCityCenter (KM)`, `Apis mellifera (Large)`))

with(Ave_Pollinator_Data, cor.test(`DistanceFromCityCenter (KM)`, sqrt_sweat_bees))
with(Ave_Pollinator_Data, cor.test(`DistanceFromCityCenter (KM)`, sqrt_leafcutter_bees))
with(Ave_Pollinator_Data, cor.test(`DistanceFromCityCenter (KM)`, sqrt_orange_beetle))
with(Ave_Pollinator_Data, cor.test(`DistanceFromCityCenter (KM)`, sqrt_apis_melifera))

with(Ave_Pollinator_Data, cor.test(`DistanceFromCityCenter (KM)`, sqrt_pollinator_abundance))
with(Ave_Pollinator_Data, cor.test(`DistanceFromCityCenter (KM)`, sqrt_bee_abundance))

with(Ave_Pollinator_Data, cor.test(`DistanceFromCityCenter (KM)`, `diversity all groups (bee families + other spp.)`))
with(Ave_Pollinator_Data, cor.test(`DistanceFromCityCenter (KM)`, sqrt_diversity_all))

with(Ave_Pollinator_Data, cor.test(`DistanceFromCityCenter (KM)`, `diversity bee families`))
with(Ave_Pollinator_Data, cor.test(`DistanceFromCityCenter (KM)`, sqrt_diversity_bees))

##linear models
pollinator_diversity_linear_model <- lm(`DistanceFromCityCenter (KM)` ~ `diversity all groups (bee families + other spp.)`, data = Ave_Pollinator_Data)
print(pollinator_diversity_linear_model)
summary(pollinator_diversity_linear_model)
plot(pollinator_diversity_linear_model)
with(Ave_Pollinator_Data, plot(`DistanceFromCityCenter (KM)`, `diversity all groups (bee families + other spp.)`, col = "blue", pch = 16))
par(new = TRUE)
abline(lm(`diversity all groups (bee families + other spp.)` ~ `DistanceFromCityCenter (KM)`, data = Ave_Pollinator_Data), col = "red")
par(new = FALSE)


pollinator_abundance_sqrt_linear_model <- lm(`DistanceFromCityCenter (KM)` ~ sqrt_pollinator_abundance, data = Ave_Pollinator_Data)
print(pollinator_abundance_sqrt_linear_model)
summary(pollinator_abundance_sqrt_linear_model)
plot(pollinator_abundance_sqrt_linear_model)
with(Ave_Pollinator_Data, plot(`DistanceFromCityCenter (KM)`, sqrt_pollinator_abundance, col = "blue", pch = 16))
par(new = TRUE)
abline(lm(sqrt_pollinator_abundance ~ `DistanceFromCityCenter (KM)`, data = Ave_Pollinator_Data), col = "red")
par(new = FALSE)

pollinator_abundance_linear_model <- lm(`DistanceFromCityCenter (KM)` ~ `abundance all sp.`, data = Ave_Pollinator_Data)
print(pollinator_abundance_linear_model)
summary(pollinator_abundance_linear_model)
plot(pollinator_abundance_linear_model)
with(Ave_Pollinator_Data, plot(`DistanceFromCityCenter (KM)`, `abundance all sp.`, col = "blue", pch = 16))
par(new = TRUE)
abline(lm(`abundance all sp.` ~ `DistanceFromCityCenter (KM)`, data = Ave_Pollinator_Data), col = "red")
par(new = FALSE)

small_pollinator_abundance_linear_model <- lm(`DistanceFromCityCenter (KM)` ~ small_pollinators, data = Ave_Pollinator_Data)
print(small_pollinator_abundance_linear_model)
summary(small_pollinator_abundance_linear_model)
plot(small_pollinator_abundance_linear_model)
with(Ave_Pollinator_Data, plot(`DistanceFromCityCenter (KM)`, small_pollinators, col = "blue", pch = 16))
par(new = TRUE)
abline(lm(small_pollinators ~ `DistanceFromCityCenter (KM)`, data = Ave_Pollinator_Data), col = "red")
par(new = FALSE)

small_pollinator_abundance_sqrt_linear_model <- lm(`DistanceFromCityCenter (KM)` ~ sqrt_small_pollinators, data = Ave_Pollinator_Data)
print(small_pollinator_abundance_sqrt_linear_model)
summary(small_pollinator_abundance_sqrt_linear_model)
plot(small_pollinator_abundance_sqrt_linear_model)
with(Ave_Pollinator_Data, plot(`DistanceFromCityCenter (KM)`, sqrt_small_pollinators, col = "blue", pch = 16))
par(new = TRUE)
abline(lm(sqrt_small_pollinators ~ `DistanceFromCityCenter (KM)`, data = Ave_Pollinator_Data), col = "red")
par(new = FALSE)


large_pollinator_abundance_linear_model <- lm(`DistanceFromCityCenter (KM)` ~ large_pollinators, data = Ave_Pollinator_Data)
print(large_pollinator_abundance_linear_model)
summary(large_pollinator_abundance_linear_model)
plot(large_pollinator_abundance_linear_model)##data too sparse?
with(Ave_Pollinator_Data, plot(`DistanceFromCityCenter (KM)`, large_pollinators, col = "blue", pch = 16))
par(new = TRUE)
abline(lm(large_pollinators ~ `DistanceFromCityCenter (KM)`, data = Ave_Pollinator_Data), col = "red")
par(new = FALSE)

large_pollinator_abundance_sqrt_linear_model <- lm(`DistanceFromCityCenter (KM)` ~ sqrt_large_pollinators, data = Ave_Pollinator_Data)
print(large_pollinator_abundance_sqrt_linear_model)
summary(large_pollinator_abundance_sqrt_linear_model)
plot(large_pollinator_abundance_sqrt_linear_model)##data too sparse?
with(Ave_Pollinator_Data, plot(`DistanceFromCityCenter (KM)`, sqrt_large_pollinators, col = "blue", pch = 16))
par(new = TRUE)
abline(lm(sqrt_large_pollinators ~ `DistanceFromCityCenter (KM)`, data = Ave_Pollinator_Data), col = "red")
par(new = FALSE)


large_bee_abundance_linear_model <- lm(`DistanceFromCityCenter (KM)` ~ large_bees, data = Ave_Pollinator_Data)
print(large_bee_abundance_linear_model)
summary(large_bee_abundance_linear_model)
plot(large_bee_abundance_linear_model)##not good model
with(Ave_Pollinator_Data, plot(`DistanceFromCityCenter (KM)`, large_bees, col = "blue", pch = 16))
par(new = TRUE)
abline(lm(large_bees ~ `DistanceFromCityCenter (KM)`, data = Ave_Pollinator_Data), col = "red")
par(new = FALSE)

large_bee_abundance_sqrt_linear_model <- lm(`DistanceFromCityCenter (KM)` ~ sqrt_large_bees, data = Ave_Pollinator_Data)
print(large_bee_abundance_sqrt_linear_model)
summary(large_bee_abundance_sqrt_linear_model)
plot(large_bee_abundance_sqrt_linear_model)
with(Ave_Pollinator_Data, plot(`DistanceFromCityCenter (KM)`, sqrt_large_bees, col = "blue", pch = 16))
par(new = TRUE)
abline(lm(sqrt_large_bees ~ `DistanceFromCityCenter (KM)`, data = Ave_Pollinator_Data), col = "red")
par(new = FALSE)##not good model


Rhagoycha_fulva_linear_model <- lm(`DistanceFromCityCenter (KM)` ~ `orange beetle (Rhagoycha fulva)`, data = Ave_Pollinator_Data)
print(Rhagoycha_fulva_linear_model)
summary(Rhagoycha_fulva_linear_model)
plot(Rhagoycha_fulva_linear_model)
with(Ave_Pollinator_Data, plot(`DistanceFromCityCenter (KM)`, `orange beetle (Rhagoycha fulva)`, col = "blue", pch = 16))
par(new = TRUE)
abline(lm(`orange beetle (Rhagoycha fulva)` ~ `DistanceFromCityCenter (KM)`, data = Ave_Pollinator_Data), col = "red")
par(new = FALSE)

Rhagoycha_fulva_sqrt_linear_model <- lm(`DistanceFromCityCenter (KM)` ~ sqrt_orange_beetle, data = Ave_Pollinator_Data)
print(Rhagoycha_fulva_sqrt_linear_model)
summary(Rhagoycha_fulva_sqrt_linear_model)
plot(Rhagoycha_fulva_sqrt_linear_model)##not good model
with(Ave_Pollinator_Data, plot(`DistanceFromCityCenter (KM)`, sqrt_orange_beetle, col = "blue", pch = 16))
par(new = TRUE)
abline(lm(sqrt_orange_beetle ~ `DistanceFromCityCenter (KM)`, data = Ave_Pollinator_Data), col = "red")
par(new = FALSE)


Apis_mellifera_linear_model <- lm(`DistanceFromCityCenter (KM)` ~ `Apis mellifera (Large)`, data = Ave_Pollinator_Data)
print(Apis_mellifera_linear_model)
summary(Apis_mellifera_linear_model)
plot(Apis_mellifera_linear_model)##bad model
with(Ave_Pollinator_Data, plot(`DistanceFromCityCenter (KM)`, `Apis mellifera (Large)`, col = "blue", pch = 16))
par(new = TRUE)
abline(lm(`Apis mellifera (Large)` ~ `DistanceFromCityCenter (KM)`, data = Ave_Pollinator_Data), col = "red")
par(new = FALSE)

Apis_mellifera_sqrt_linear_model <- lm(`DistanceFromCityCenter (KM)` ~ sqrt_apis_melifera, data = Ave_Pollinator_Data)
print(Apis_mellifera_sqrt_linear_model)
summary(Apis_mellifera_sqrt_linear_model)
plot(Apis_mellifera_sqrt_linear_model)
with(Ave_Pollinator_Data, plot(`DistanceFromCityCenter (KM)`, sqrt_apis_melifera, col = "blue", pch = 16))
par(new = TRUE)
abline(lm(sqrt_apis_melifera ~ `DistanceFromCityCenter (KM)`, data = Ave_Pollinator_Data), col = "red")
par(new = FALSE)

##pollinator_diversity_sqrt_linear_model <- lm(`DistanceFromCityCenter (KM)` ~ sqrt_diversity_all, data = Ave_Pollinator_Data)
##print(pollinator_diversity_sqrt_linear_model)
##summary(pollinator_diversity_sqrt_linear_model)
##plot(pollinator_diversity_sqrt_linear_model)

##bee_diversity_linear_model <- lm(`DistanceFromCityCenter (KM)` ~ `diversity bee families`, data = Ave_Pollinator_Data)
##print(bee_diversity_linear_model)
##summary(bee_diversity_linear_model)
##plot(bee_diversity_linear_model)

aggregate(Ave_Pollinator_New, list(Ave_Pollinator_New$Type), boxplot)

with(Ave_Pollinator_New, boxplot(split(abundance.all.sp., Type)))
with(Ave_Pollinator_New, boxplot(split(leaf.cutter.bees..Megachile.sp.., Type)))
with(Ave_Pollinator_New, boxplot(split(sweat.bees..Halictidae.sp.., Type)))
with(Ave_Pollinator_New, boxplot(split(orange.beetle..Rhagoycha.fulva., Type)))
with(Ave_Pollinator_New, boxplot(split(Hylaeus.spp., Type)))
with(Ave_Pollinator_New, boxplot(split(Xylocopinae.sp., Type)))
with(Ave_Pollinator_New, boxplot(split(Anthophorinae.spp., Type)))
with(Ave_Pollinator_New, boxplot(split(Colletes.spp., Type)))









