# Bid Rent Curves for CBSAs

Bid rent curves are a useful way to visualize the degree to which a city adheres to a monocentric model. I calculated bid-rent curves for the 100 largest Core-based statistical areas in the United States using American Community Survey data. A key consideration when calculating bid-rent curves is determining the center of a given CBSA. Following the advice in [this article](https://www.huduser.gov/portal/periodicals/cityscpe/vol21num2/ch12.pdf), I used city center definitions from Wilson et al. (2012), who based their definition on the address of each city’s city hall. I pulled all tracts in a given CBSA using `tidycensus`, filtered to those within a given radius from the city center, and then calculated the distance from the city center using tract centroids.

You can view my bid rent curve visualizations for every US city [here](https://camdenblatchly.com/posts/bid-rent-curves).
