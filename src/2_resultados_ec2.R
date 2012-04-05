library(ProjectTemplate)
load.project()


nrow(primarias.s)
nrow(primarias.s.2)

dim(sims.pbound.sample)

sims.pbound.2 <- sims.pbound.sample[seq(1,390,2),]
sims.m <- melt(sims.pbound.2)

names(sims.m) <- c("rep", "id.escuela", "value")

write.csv(sims.m, file = "./out/sims_m.csv")

primarias.s$id.escuela <- 1:76081
write.csv(primarias.s, file="./out/primarias_s.csv")