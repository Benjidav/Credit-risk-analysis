#Method 2 : Subset selection + Logistic regression (exception of dr, rev : glm.fit: the algorithm did not converge )
model.null <- glm(formula = dlq ~ 1, family = "binomial", data = training_set )

model.full <- glm(formula = dlq ~ Age + nb30 + nb60 + nb90 + mi + de + re + nc, family = "binomial", data = training_set )

stepwise <- step(model.null,scope = list(upper=model.full), direction="both",test="Chisq",data=training_set)

summary(stepwise)

#Kaiser-Criteria : Only Principal Component with lambda greater than 1 are stored
count.component = function (eigen)
{
  counter = 0
  for (i in  1 : length(eigen)) 
  {
    if (round(eigen[i], 2) > 1)
    {
      counter = counter + 1
    }
  }
  return (counter)
}

