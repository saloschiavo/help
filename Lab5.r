##STAT 435: LAB
##Air pollution data (import)
##NO3T--> response (y); remaining are predictors

# in order to bring in our dataset, we do NOT overwork ourselves and try a read csv function
# and then try to figure out if we should put it in a dataframe because this isn't our stupid CS class
# instead, R Studio does this for us very easily
# all we have to do is click on Import Dataset under ENVIRONMENT
# and then click on the button that enables Headers so that our dataset automatically gets labels
# honestly, bless R

# so here we're trying to see how everything affects NO3T
# so we use ~. for all variables as our x's
m1=lm(NO3T~., data=Data1)
summary(m1)

# to trust these conclusions, we have to set up model diagnostics!
# we can do that by using plot() and plotting the model object m1
plot(m1)

# note that the residuals vs fitted plot, our residual plot,
# definitely has some objects that look like they could be outliers
# so now we have to quantify those outliers, confirm that they are indeed outliers
# to do this, we use studres() function
# if you have any trouble running this function -- if R says it can't find it --
# make sure that you have MASS package installed under "Packages" --> System Library

###find outliers
studentized_resi=studres(m1)
studentized_resi

# now let's plot those residuals
# note that most behave roughly similarly, but there are definitely some outliers
plot(studentized_resi)
# any deviation beyond +- 3 is an outlier
# so let's remove these indices!
# remember that each of these correspond to a row in the dataset
# so figure out which rows correspond to these values
# this way we can eliminate them
# which values of the absolute values of all studentized_resi are greater than 3
outlier_indices=which(abs(studentized_resi)>3)
# this gives us the indices
outlier_indices

# the 64th observation here is an issue

##find high leverage points
#we do this by using Cook's Distance, so we use the built-in function
cd=cooks.distance(m1)
cd

# so how do we use cooks distance? we have to see
##the points where cooks distance is >4/n --> high leverage
# so let's extract these points
# how many observations are in data set?
n=nrow(Data1)
n
# n = 150
# then let's extract the leverage indices -- the points where the cd is > 4/n
leverage_indices=which(cd>4/n)
leverage_indices
# note that some of these overlap with our outliers
# remember that cook's distance features both outliers and high leverage points
# so this makes sense!
# now we have to combine these indices, so we have to:

##remove outliers and high leverage points
# we're going to store these into a remove_indices variable
remove_indices=union(outlier_indices,leverage_indices)
remove_indices

###check for high collinearity (this can be done using
##vif)
# in order to use vif(), we have to bring in the car package
# You can also go over to Packages and select car
# it'll also auto-install carData too, it's cool
# if vif > 5, then we worry about high collinearity
library(car)
vif(m1)
??vif
###vif check indicates no problems with outliers since vf is not > 5

# now we know we just have to remove the earlier indices of outliers
# and high leverage values
# we make a new dataset out of the old one, only keeping the indices
# that don't have crazy values, so this is how we do that:
newdata=Data1[-remove_indices, ]

# now we'll make a second model, m2 to do proper analysis without
# those high leverage points and outliers
# note that it has slightly fewer observations than before because
# we have removed those values
m2=lm(NO3T~.,data=newdata)
summary(m2)
plot(m2)
# now we have seen that some things -- AG_500 and URB_500 --
# have become significant
# so let's recheck our model diagnostics since they may look
# more reasonable
# but note there's still another problem here:
##residual plot for new model
# this plots fitted values on x axis, residuals on y axis
plot(m2$fitted.values,m2$residuals)
# but this plot is still not ideal. the points that are super far
# are still not outliers based on our residual
# the problem we're seeing here is heteroskedasticity
# the spread increases, we can see non-constant variance

##so our residual plot still indicates a non-constant variance
# so the solution to that is using a log transformation!
# so let's go ahead and do that with m3, our final model
# here we use NO3T as our Y, and use ~. to do multiple regression again
m3=lm(log(NO3T)~., data=newdata)
summary(m3)
# now note that more things have also become significant
# and here's the most important plot, again, our residual plot
# which plots fitted values against the residuals:
# now we see the plot is much more believable
# things are now distributed mostly randomly, which is good!
plot(m3$fitted.values,m3$residuals)
