#Examples
data(school23)
model <- lmer(math ~ structure + SES + (1 | school.ID), data=school23)
alt.est <- influence(model, group="school.ID")
cooks.distance(alt.est)
