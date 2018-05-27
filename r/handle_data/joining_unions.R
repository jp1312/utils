
## --- Base 

# Inner join
merge(df1, df2)

# Outer join
merge(x = df1, y = df2, by = "CustomerId", all = TRUE)

# Left outer
merge(x = df1, y = df2, by = "CustomerId", all.x = TRUE)

# Right outer
merge(x = df1, y = df2, by = "CustomerId", all.y = TRUE)

# Cross join
merge(x = df1, y = df2, by = NULL)