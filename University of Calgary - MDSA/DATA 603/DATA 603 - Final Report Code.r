library(mctest)
library(olsrr)
soccer <- read.csv("C:/Users/dingj/Downloads/final603data.csv")
soccer
model = lm(overall_rating~height+weight+preferred_foot+attacking_work_rate+defensive_work_rate+crossing+finishing+heading_accuracy+short_passing+volleys+dribbling+curve+free_kick_accuracy+long_passing+ball_control+acceleration+sprint_speed+agility+reactions+balance+shot_power+jumping+stamina+strength+long_shots+aggression+interceptions+positioning+vision+penalties+marking+standing_tackle+sliding_tackle+age,data=soccer)
summary(model)

stepmod=ols_step_both_p(model,p_enter = 0.05, p_remove = 0.1, details=TRUE)
summary(stepmod$model)

stepmod$metrics

anova_result <- anova(stepmod$model, model)
print(anova_result)

anova_result <- anova(reducedmodel, model)
print(anova_result)


imcdiag(model, method="VIF")

reducedmodel = lm(overall_rating~height+weight+preferred_foot+attacking_work_rate+defensive_work_rate+crossing+finishing+heading_accuracy+short_passing+volleys+dribbling+curve+free_kick_accuracy+long_passing+ball_control+acceleration+sprint_speed+agility+reactions+balance+shot_power+jumping+stamina+strength+long_shots+aggression+interceptions+positioning+vision+penalties+age,data=soccer)
summary(reducedmodel)

bestlinearmodel = lm(overall_rating~height+weight+attacking_work_rate+defensive_work_rate+crossing+finishing+heading_accuracy+short_passing+curve+long_passing+ball_control+acceleration+sprint_speed+agility+reactions+shot_power+jumping+strength+long_shots+interceptions+positioning+vision+age,data=soccer)
summary(bestlinearmodel)

anova_result <- anova(bestlinearmodel, reducedmodel)
print(anova_result)

interactmodel = lm(overall_rating~(height+weight+attacking_work_rate+defensive_work_rate+crossing+finishing+heading_accuracy+short_passing+curve+long_passing+ball_control+acceleration+sprint_speed+agility+reactions+shot_power+jumping+strength+long_shots+interceptions+positioning+vision+age)^2,data=soccer)
summary(interactmodel)

anova_result <- anova(interactmodel, bestlinearmodel)
print(anova_result)

adjinteractmodel = lm(overall_rating~height+weight+attacking_work_rate+defensive_work_rate+crossing+finishing+heading_accuracy+short_passing+curve+long_passing+ball_control+acceleration+sprint_speed+agility+reactions+shot_power+jumping+strength+long_shots+interceptions+positioning+vision+age+height*attacking_work_rate+height*crossing+height*finishing+height*shot_power+height*long_shots+height*interceptions+weight*finishing+weight*positioning+attacking_work_rate*finishing+attacking_work_rate*heading_accuracy+attacking_work_rate*curve+attacking_work_rate*sprint_speed+attacking_work_rate*reactions+defensive_work_rate*agility+defensive_work_rate*interceptions+defensive_work_rate*positioning+crossing*finishing+crossing*heading_accuracy+crossing*sprint_speed+crossing*shot_power+crossing*jumping+crossing*vision+finishing*heading_accuracy+finishing*short_passing+ finishing*interceptions+finishing*positioning+heading_accuracy*short_passing+heading_accuracy*long_passing+heading_accuracy*ball_control+heading_accuracy*acceleration+heading_accuracy*sprint_speed+heading_accuracy*agility+heading_accuracy*reactions+heading_accuracy*shot_power+heading_accuracy*jumping+heading_accuracy*strength+heading_accuracy*long_shots,data=soccer)
summary(adjinteractmodel)

anova_result <- anova(adjinteractmodel, interactmodel)
print(anova_result)

bestlinearmodelpower = lm(overall_rating~height+weight+attacking_work_rate+defensive_work_rate+crossing+finishing+heading_accuracy+short_passing+curve+long_passing+ball_control+acceleration+sprint_speed+agility+reactions+shot_power+jumping+strength+long_shots+interceptions+positioning+vision+age,data=soccer)
summary(bestlinearmodelpower)

anova_result <- anova(bestlinearmodelpower, adjinteractmodel)
print(anova_result)

plot(adjinteractmodel, which = 1)

shapiro.test(residuals(adjinteractmodel))

residuals <- residuals(adjinteractmodel)
sampled_residuals <- sample(residuals, size = 5000, replace = FALSE)
shapiro_test <- shapiro.test(sampled_residuals)
print(shapiro_test)

hist(residuals(adjinteractmodel))

qqnorm(residuals, main = "Q-Q Plot of Residuals")
qqline(residuals)

plot(adjinteractmodel,which=1)

library(MASS)
bc=boxcox(adjinteractmodel,lambda=seq(-2,2))
bestlambda=bc$x[which(bc$y==max(bc$y))]
bestlambda

bcmodel=lm((((overall_rating^bestlambda)-1)/bestlambda)~height+weight+attacking_work_rate+defensive_work_rate+crossing+finishing+heading_accuracy+short_passing+curve+long_passing+ball_control+acceleration+sprint_speed+agility+reactions+shot_power+jumping+strength+long_shots+interceptions+positioning+vision+age+height*attacking_work_rate+height*crossing+height*finishing+height*shot_power+height*long_shots+height*interceptions+weight*finishing+weight*positioning+attacking_work_rate*finishing+attacking_work_rate*heading_accuracy+attacking_work_rate*curve+attacking_work_rate*sprint_speed+attacking_work_rate*reactions+defensive_work_rate*agility+defensive_work_rate*interceptions+defensive_work_rate*positioning+crossing*finishing+crossing*heading_accuracy+crossing*sprint_speed+crossing*shot_power+crossing*jumping+crossing*vision+finishing*heading_accuracy+finishing*short_passing+ finishing*interceptions+finishing*positioning+heading_accuracy*short_passing+heading_accuracy*long_passing+heading_accuracy*ball_control+heading_accuracy*acceleration+heading_accuracy*sprint_speed+heading_accuracy*agility+heading_accuracy*reactions+heading_accuracy*shot_power+heading_accuracy*jumping+heading_accuracy*strength+heading_accuracy*long_shots,data=soccer)
summary(bcmodel)

# Load the lmtest package for the bptest function
library(lmtest)
bp_test <- bptest(bcmodel)
print(bp_test)
summary(bcmodel)

bcmodel1=lm(log(overall_rating)~height+weight+attacking_work_rate+defensive_work_rate+crossing+finishing+heading_accuracy+short_passing+curve+long_passing+ball_control+acceleration+sprint_speed+agility+reactions+shot_power+jumping+strength+long_shots+interceptions+positioning+vision+age+height*attacking_work_rate+height*crossing+height*finishing+height*shot_power+height*long_shots+height*interceptions+weight*finishing+weight*positioning+attacking_work_rate*finishing+attacking_work_rate*heading_accuracy+attacking_work_rate*curve+attacking_work_rate*sprint_speed+attacking_work_rate*reactions+defensive_work_rate*agility+defensive_work_rate*interceptions+defensive_work_rate*positioning+crossing*finishing+crossing*heading_accuracy+crossing*sprint_speed+crossing*shot_power+crossing*jumping+crossing*vision+finishing*heading_accuracy+finishing*short_passing+ finishing*interceptions+finishing*positioning+heading_accuracy*short_passing+heading_accuracy*long_passing+heading_accuracy*ball_control+heading_accuracy*acceleration+heading_accuracy*sprint_speed+heading_accuracy*agility+heading_accuracy*reactions+heading_accuracy*shot_power+heading_accuracy*jumping+heading_accuracy*strength+heading_accuracy*long_shots,data=soccer)
summary(bcmodel1)

bp_test <- bptest(bcmodel)
print(bp_test)

imcdiag(adjinteractmodel, method="VIF")
summary(adjinteractmodel)

VIFMODEL1=lm(overall_rating~height+weight+defensive_work_rate+heading_accuracy+short_passing+curve+long_passing+ball_control+acceleration+sprint_speed+agility+reactions+jumping+strength+positioning+vision+age+weight*finishing+weight*positioning+defensive_work_rate*agility+defensive_work_rate*interceptions+defensive_work_rate*positioning+heading_accuracy*short_passing+heading_accuracy*long_passing+heading_accuracy*ball_control+heading_accuracy*acceleration+heading_accuracy*sprint_speed+heading_accuracy*agility+heading_accuracy*reactions+heading_accuracy*shot_power+heading_accuracy*jumping+heading_accuracy*strength,data=soccer)
imcdiag(VIFMODEL1, method="VIF")
summary(VIFMODEL1)

VIFMODEL2=lm(overall_rating~height+weight+defensive_work_rate+heading_accuracy+short_passing+curve+long_passing+ball_control+acceleration+sprint_speed+agility+reactions+jumping+strength+vision+age+defensive_work_rate*agility+defensive_work_rate*interceptions+defensive_work_rate*positioning+heading_accuracy*long_passing+heading_accuracy*agility+heading_accuracy*reactions+heading_accuracy*shot_power+heading_accuracy*jumping+heading_accuracy*strength,data=soccer)
imcdiag(VIFMODEL2, method="VIF")

summary(VIFMODEL2)

plot(VIFMODEL2,which=5)
plot(VIFMODEL2,which=4)

plot(residuals(adjinteractmodel), main = "Residual Plot for adjinteractmodel", xlab = "Observation", ylab = "Residuals")

