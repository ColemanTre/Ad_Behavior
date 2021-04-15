#Ask about types of ads they plan on pushing

#Step 1- Did they see the ad?
generate a distribution for all the users and generate a probability distribution for them seeing the ad

#Step 2 - Did they interact with the ad?
of the users who saw the ad, generate a probability distribution of the users who interacted with the ad

There will be different distributions for different offering types. 

We won't be able to draw conclusions about other variables like location 
and emotional intensity because the data is random.
There shouldn't be any correlation at all.

    This is the amount of correlation we would expect based on the randomness of the data. 
    Form a hypothesis and make assumptions and perform analysis on assumptions.
          
          There are a lot of assumptions we can try to work with. Location is the one that is most on my mind
          because I can see that as a big factor in influecing ad interactions. We could go for assumptions about
          the country they live in. (Groupby) The state they live in. 

Create the data in a way that it's easy to change and incorporate assumptions'

data = read.csv('bq-results-20201013-133031-gi2msesmnqa8.csv')

#What data do you want me to use?
