# We have two ontologies which can be mapped to a third ontology,
# but "get.mappings.to.align.ontologies" can't find the solution

ont1 = ontology(age = c("0-14 years",
                        "15-19 years",
                        "20-24 years",
                        "25-29 years",
                        "30-34 years",
                        "35-44 years",
                        "45+ years"))
ont2 = ontology(age = c("0-14 years",
                        "15-24 years",
                        "25-34 years",
                        "35-44 years",
                        "45-54 years",
                        "55-64 years",
                        "65+ years"))
# gets NULL
get.mappings.to.align.ontologies(ont1, ont2)

# We know there's a third ontology that each can map directly to
ont3 = ontology(age = c("0-14 years",
                        "15-24 years",
                        "25-34 years",
                        "35-44 years",
                        "45+ years"))

get.ontology.mapping(ont1, ont3)
get.ontology.mapping(ont2, ont3)
