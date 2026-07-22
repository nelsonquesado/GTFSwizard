# Print, Summarize, and Plot \`wizardgtfs\` Objects

Print, Summarize, and Plot \`wizardgtfs\` Objects

## Usage

``` r
# S3 method for class 'wizardgtfs'
print(x, ..., n = 5L)

# S3 method for class 'wizardgtfs'
summary(object, ...)

# S3 method for class 'summary.wizardgtfs'
print(x, ...)

# S3 method for class 'wizardgtfs'
plot(x, ...)
```

## Arguments

- x, object:

  A \`wizardgtfs\` object or its summary.

- ...:

  Additional arguments passed to print methods.

- n:

  Number of rows shown per GTFS table.

## Value

\`print()\` returns \`x\` invisibly; \`summary()\` returns a
\`summary.wizardgtfs\` object; \`plot()\` returns a \`ggplot\` object.
