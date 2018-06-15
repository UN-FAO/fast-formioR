# FAST - formioR

A small R library to ease your work with R.
A wraper Class on top of the Form.io API

### Installing

To install this package in your R project, you will need to have the 'devtools' package (soon we will move it to CRAN)

```
install.packages("devtools")
library("devtools")
```

Then you can install this directly from Github using

```
install_github('fast-formioR','UN-FAO')
```

# Usage

Until now we only support the GET HTTP request, as we mainly use this library to pull data for statistics, the package works in the following way

# Quick Example

No time to read? No worries, here you have a quick example of all you can do

```R
token <- "P9reVWfdJiI5rc7qre4bn2HVcdyJJi1"
url <- "https://myAPI.form.io/"
User <- formior::Formior$new(url, "user", token)
filter <- list(list('_id','!=','5a65acec16987c0001f3d246'), list('owner','!=','2342342344'))
select <- list('data.a', 'owner', '_id')
limit <- 150
chunk_size <- 15
populate <- list('owner')

# Apply the filter, limit
# All methods except get() return the self object (this)
User$select(select)$filter(filter)$limit(limit)$populate(populate)$chunks(chunk_size)

# If you want you can also do it the "R" way and be a little more verbose about it
User$select(select)
User$filter(filter)
User$limit(limit)
User$populate(populate)
User$chunks(chunk_size)

#Once you are ready, just pull your results with get()
data <- User$get()
```

## The FormioR instance

```R
User <- Formior$new(
			base_url,      #Base URL of your Form.io project
			resource_name, #The API path of the resource or Form that you want to use
			token          # token or jwt-token for Auth
		)
```

Once you have your instance you can go and Query the resource by using chained methods. The available methods are the following

## Methods

| Method   | Parameter Type           | Description                                                      |
| -------- | ------------------------ | ---------------------------------------------------------------- |
| select   | list()                   | List of all the attributes you want to pull                      |
| filter   | list(list(), list(),...) | A list of all filter queries (See filter section)                |
| limit    | numeric                  | The max amount of elements to pull                               |
| populate | list()                   | List of all linked resources to populate                         |
| chunks   | numeric                  | How many elements to pull at the time (usefull for big datasets) |
| get      | NONE                     | Applies all methods and generates the query                      |

## Filter method

The filter method receives a list with filters inside. Every filter is also a list defined in the following way:

```R
filter <- list(
	list(
		'attribute',       #The attribute of our Resource or Form to filter
		'operator/query',  #The operator i.e= '=' , '!=' (See list of Queries)
		'value'            #The operator will check the attribute against this value
	)
)
```

You can have as many filters as you want, but know that all filters will follow the "and" operator (&&)
so if you apply two filters they will work as: Filter1 && Filter2. The or operator is not yet supported
for multiple filters

## Operators

| Operator                 | Query   | Example                                   |
| ------------------------ | ------- | ----------------------------------------- |
| Equal                    | =       | list('data.gender', '=', 'male')          |
| Not equal                | !=      | list('data.gender', '!=', 'male')         |
| Greater Tthan            | >       | list('data.age', '>', '18')               |
| Greater than or equal to | >=      | list('data.age', '>=', '18')              |
| Less than                | <       | list('data.age', '<', '30')               |
| Less than or equal to    | <=      | list('data.age', '<=', '30')              |
| In                       | in      | list('data.gender', 'in', 'male,female')  |
| Not in                   | nin     | list('data.age', 'nin', '18,21')          |
| Exists                   | exists  | list('data.age', 'exists')                |
| Not exists               | !exists | list('data.age', '!exists')               |
| Regex                    | regex   | list('data.username', 'regex', '/^cab/i') |
