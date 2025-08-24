mod0 <- plm(productivity ~ lag(job_variation), 
            data=df, model="within", effect="twoways")
summary(mod0)

mod1 <- plm(job_variation ~ TER + TER_MAQ, 
                 data=df, model="within", effect="twoways")
summary(mod1)

mod2 <- plm(productivity ~ lag(job_variation)| 
            lag(TER) + lag(TER_MAQ), 
            data=df, model="within", effect="twoways")
summary(mod2)

