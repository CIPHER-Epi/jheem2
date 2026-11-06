# Tests that a pull correctly merges metadata when it aggregates data points that came from puts with different metadata.
# Covers aggregation over a dimension, ontology mappings, age restratification, NA points, fields missing from some records, and the separator.
# Run from the jheem2 directory, after pkgload::load_all() or library(jheem2)

#-- Helpers --#
check = function(description, actual, expected)
{
    if (!identical(actual, expected)) {
        cat("FAILED:", description, "\n  expected: "); dput(expected)
        cat("  actual:   "); dput(actual)
        stop("Test failed: ", description)
    }
}

#-- Set up a data manager --#
dm = create.data.manager('metadata.aggregation.test', description = 'a data manager to test metadata aggregation')
dm$register.outcome('new', metadata = create.outcome.metadata(scale = 'non.negative.number',
                                                              display.name = 'New Diagnoses',
                                                              axis.name = 'New Diagnoses (n)',
                                                              units = 'cases',
                                                              description = 'New HIV Cases Diagnosed in a Year'))
dm$register.parent.source('cdc', 'cdc', 'cdc')
dm$register.source('cdc', 'cdc', full.name = 'US Centers for Disease Control and Prevention', short.name = 'CDC')
dm$register.ontology('ont', ont = ontology(location = NULL, year = NULL, sex = c('male', 'female')))
dm$register.metadata.field('author')
dm$register.metadata.field('year')

dn = list(location = c('C.12580', 'C.26420'), year = c('2020', '2021'), sex = 'male')
male = array(1:4, sapply(dn, length), dn)
female = male + 10
dimnames(female)$sex = 'female'

# Record 1: male
dm$put(male, outcome = 'new', source = 'cdc', ontology.name = 'ont',
       metadata = list(url = 'http://a.gov', details = 'Method A', author = 'Smith', year = 2021))
# Record 2: female. It shares one url and one author with record 1 and has no 'year'
dm$put(female, outcome = 'new', source = 'cdc', ontology.name = 'ont',
       metadata = list(url = c('http://b.gov', 'http://a.gov'), details = 'Method B', author = c('Smith', 'Jones')))

#-- 1. Aggregating over sex merges the two records --#
agg = dm$pull('new', keep.dimensions = c('location', 'year'), dimension.values = list(sex = c('male', 'female')),
              append.attributes = c('url', 'details', 'author', 'year', 'metadata'))

check("aggregated data are summed", as.vector(agg[, , 'cdc']), as.numeric(1:4 + 11:14))
for (loc in dn$location) for (yr in dn$year) {
    cell = paste(loc, yr)
    check(paste("url, sorted and without duplicates,", cell), attr(agg, 'url')[loc, yr, 'cdc'], 'http://a.gov | http://b.gov')
    check(paste("details,", cell), attr(agg, 'details')[loc, yr, 'cdc'], 'Method A | Method B')
    check(paste("author, sorted and without duplicates,", cell), attr(agg, 'author')[loc, yr, 'cdc'], 'Jones | Smith')
    check(paste("year, from the only record that has it,", cell), attr(agg, 'year')[loc, yr, 'cdc'], '2021')
    check(paste("metadata.ids,", cell), attr(agg, 'metadata.ids')[[loc, yr, 'cdc']], 1:2)
}
check("metadata.ids has the shape of the data", dim(attr(agg, 'metadata.ids')), dim(agg))
check("metadata.ids has the dimnames of the data", dimnames(attr(agg, 'metadata.ids')), dimnames(agg))
check("field attributes have the shape of the data", dim(attr(agg, 'author')), dim(agg))
check("metadata table lists both records", attr(agg, 'metadata')$id, 1:2)
check("metadata table url column", attr(agg, 'metadata')$url, c('http://a.gov', 'http://a.gov | http://b.gov'))
check("metadata table year column", attr(agg, 'metadata')$year, c('2021', NA))

#-- 2. Without aggregation, each cell keeps only its own record --#
by.sex = dm$pull('new', keep.dimensions = c('location', 'year', 'sex'), append.attributes = c('url', 'metadata'))
check("male url, unaggregated", attr(by.sex, 'url')['C.12580', '2020', 'male', 'cdc'], 'http://a.gov')
check("female url, unaggregated", attr(by.sex, 'url')['C.12580', '2020', 'female', 'cdc'], 'http://a.gov | http://b.gov')
check("male metadata.ids, unaggregated", attr(by.sex, 'metadata.ids')[['C.12580', '2020', 'male', 'cdc']], 1L)
check("female metadata.ids, unaggregated", attr(by.sex, 'metadata.ids')[['C.12580', '2020', 'female', 'cdc']], 2L)

#-- 3. Aggregating over more than one dimension merges records from every contributing point --#
# Record 3: overwrite one male cell with new metadata
dm$put(array(100, c(1, 1, 1), list(location = 'C.26420', year = '2021', sex = 'male')), outcome = 'new', source = 'cdc',
       ontology.name = 'ont', metadata = list(url = 'http://c.gov', details = 'Method C', author = 'Lee'))
agg.all = dm$pull('new', keep.dimensions = 'year',
                  dimension.values = list(location = c('C.12580', 'C.26420'), sex = c('male', 'female')),
                  append.attributes = c('url', 'author', 'metadata'))
check("2020 combines records 1 and 2", attr(agg.all, 'metadata.ids')[['2020', 'cdc']], 1:2)
check("2021 combines records 1, 2 and 3", attr(agg.all, 'metadata.ids')[['2021', 'cdc']], 1:3)
check("2021 url", attr(agg.all, 'url')['2021', 'cdc'], 'http://a.gov | http://b.gov | http://c.gov')
check("2021 author", attr(agg.all, 'author')['2021', 'cdc'], 'Jones | Lee | Smith')
check("metadata table lists all three records", attr(agg.all, 'metadata')$id, 1:3)

#-- 4. NA points do not contribute metadata --#
# Record 4: remove one male cell
dm$put(array(NA_real_, c(1, 1, 1), list(location = 'C.12580', year = '2020', sex = 'male')), outcome = 'new', source = 'cdc',
       ontology.name = 'ont', url = 'http://removed.gov', details = 'removed', allow.na.to.overwrite = T)

agg.keep.na = dm$pull('new', keep.dimensions = c('location', 'year'), dimension.values = list(sex = c('male', 'female')),
                      append.attributes = c('url', 'metadata'))
check("without na.rm, the aggregate is NA", agg.keep.na['C.12580', '2020', 'cdc'], NA_real_)
check("without na.rm, the url is NA", attr(agg.keep.na, 'url')['C.12580', '2020', 'cdc'], NA_character_)
check("without na.rm, metadata.ids are NA", attr(agg.keep.na, 'metadata.ids')[['C.12580', '2020', 'cdc']], NA_integer_)

agg.na.rm = dm$pull('new', keep.dimensions = c('location', 'year'), dimension.values = list(sex = c('male', 'female')),
                    na.rm = T, append.attributes = c('url', 'metadata'))
check("with na.rm, the aggregate is the female value", agg.na.rm['C.12580', '2020', 'cdc'], 11)
check("with na.rm, only the female record contributes", attr(agg.na.rm, 'metadata.ids')[['C.12580', '2020', 'cdc']], 2L)
check("with na.rm, the removal record's url is left out", attr(agg.na.rm, 'url')['C.12580', '2020', 'cdc'], 'http://a.gov | http://b.gov')
check("other cells are unaffected", attr(agg.na.rm, 'metadata.ids')[['C.26420', '2021', 'cdc']], 2:3)
check("the removal record is not in the metadata table", 4L %in% attr(agg.na.rm, 'metadata')$id, FALSE)

#-- 5. Aggregating through an ontology mapping merges records --#
register.ontology.mapping('metadata.test.race.mapping',
                          mappings = rbind(c('black', 'black'),
                                           c('hispanic', 'other'),
                                           c('other', 'other')),
                          from.dimensions = 'race',
                          to.dimensions = 'race')
dm$register.ontology('fine.race', ont = ontology(location = NULL, year = NULL, race = c('black', 'hispanic', 'other')))
race.dn = list(location = 'C.12580', year = '2020', race = 'black')
dm$put(array(1, c(1, 1, 1), race.dn), outcome = 'new', source = 'cdc', ontology.name = 'fine.race',
       url = 'http://black.gov', details = 'Race method')
race.dn$race = 'hispanic'
dm$put(array(2, c(1, 1, 1), race.dn), outcome = 'new', source = 'cdc', ontology.name = 'fine.race',
       url = 'http://hispanic.gov', details = 'Race method')
race.dn$race = 'other'
dm$put(array(3, c(1, 1, 1), race.dn), outcome = 'new', source = 'cdc', ontology.name = 'fine.race',
       url = 'http://other.gov', details = 'Race method')

mapped = dm$pull('new', keep.dimensions = 'race', dimension.values = list(location = 'C.12580', year = '2020'),
                 from.ontology.names = 'fine.race',
                 target.ontology = ontology(location = 'C.12580', year = '2020', race = c('black', 'other')),
                 append.attributes = c('url', 'details'))
check("mapped data are summed", as.vector(mapped[c('black', 'other'), 'cdc']), c(1, 5))
check("a category that maps one-to-one keeps its url", attr(mapped, 'url')['black', 'cdc'], 'http://black.gov')
check("a merged category combines urls", attr(mapped, 'url')['other', 'cdc'], 'http://hispanic.gov | http://other.gov')
check("a shared value appears once", attr(mapped, 'details')['other', 'cdc'], 'Race method')

#-- 6. The separator can be chosen with 'metadata.collapse' --#
agg.old.sep = dm$pull('new', keep.dimensions = 'year',
                      dimension.values = list(location = c('C.12580', 'C.26420'), sex = c('male', 'female')),
                      metadata.collapse = '__', append.attributes = c('url', 'author', 'metadata'))
check("url with '__'", attr(agg.old.sep, 'url')['2021', 'cdc'], 'http://a.gov__http://b.gov__http://c.gov')
check("author with '__'", attr(agg.old.sep, 'author')['2021', 'cdc'], 'Jones__Lee__Smith')
check("metadata table with '__'", attr(agg.old.sep, 'metadata')$url[attr(agg.old.sep, 'metadata')$id == 2], 'http://a.gov__http://b.gov')
agg.default.sep = dm$pull('new', keep.dimensions = 'year',
                          dimension.values = list(location = c('C.12580', 'C.26420'), sex = c('male', 'female')),
                          append.attributes = c('url', 'author', 'metadata'))
check("url with the default ' | '", attr(agg.default.sep, 'url')['2021', 'cdc'], 'http://a.gov | http://b.gov | http://c.gov')
check("metadata.ids do not depend on the separator", attr(agg.old.sep, 'metadata.ids'), attr(agg.default.sep, 'metadata.ids'))
check("pull.data passes the separator through",
      attr(pull.data(dm, 'new', keep.dimensions = 'year', dimension.values = list(location = 'C.26420', sex = c('male', 'female')),
                     metadata.collapse = '\n', append.attributes = 'url'), 'url')['2021', 'cdc'],
      'http://a.gov\nhttp://b.gov\nhttp://c.gov')
bad.sep.error = tryCatch({ dm$pull('new', keep.dimensions = 'year', metadata.collapse = c('a', 'b'), append.attributes = 'url'); NULL },
                         error = function(e) conditionMessage(e))
check("an invalid separator is rejected", !is.null(bad.sep.error) && grepl("metadata.collapse", bad.sep.error), TRUE)

#-- 7. Restratifying ages in pull.age.robust merges the records of overlapping brackets --#
ages = c('13-24 years', '25-34 years', '35-44 years')
dm$register.ontology('age.ont', ont = ontology(location = NULL, year = NULL, age = ages))
for (i in seq_along(ages))
    dm$put(array(c(120, 100, 100)[i], c(1, 1, 1), list(location = 'C.12580', year = '2020', age = ages[i])), outcome = 'new',
           source = 'cdc', ontology.name = 'age.ont', url = paste0('http://age', i, '.gov'), details = 'Age method')
restratified = dm$pull.age.robust('new', keep.dimensions = 'age', dimension.values = list(location = 'C.12580', year = '2020'),
                                  from.ontology.names = 'age.ont', append.attributes = c('url', 'details', 'metadata'),
                                  restratify.age = T, desired.age.brackets = c('13-29 years', '30-44 years'))
check("a new bracket gets the urls of both old brackets it overlaps (13-29)", attr(restratified, 'url')['13-29 years', 'cdc'],
      'http://age1.gov | http://age2.gov')
check("a new bracket gets the urls of both old brackets it overlaps (30-44)", attr(restratified, 'url')['30-44 years', 'cdc'],
      'http://age2.gov | http://age3.gov')
check("a shared detail appears once after restratifying", as.vector(attr(restratified, 'details')), rep('Age method', 2))
check("metadata.ids have the shape of the restratified data", dim(attr(restratified, 'metadata.ids')), dim(restratified))
check("the metadata table lists the three age records", attr(restratified, 'metadata')$url, paste0('http://age', 1:3, '.gov'))

restratified.url.only = dm$pull.age.robust('new', keep.dimensions = 'age', dimension.values = list(location = 'C.12580', year = '2020'),
                                           from.ontology.names = 'age.ont', append.attributes = 'url', metadata.collapse = '__',
                                           restratify.age = T, desired.age.brackets = c('13-29 years', '30-44 years'))
check("only the requested attribute is returned", intersect(names(attributes(restratified.url.only)), c('url', 'details', 'metadata.ids', 'metadata')), 'url')
check("restratified urls use the requested separator", attr(restratified.url.only, 'url')['13-29 years', 'cdc'], 'http://age1.gov__http://age2.gov')

restratified.no.attributes = dm$pull.age.robust('new', keep.dimensions = 'age', dimension.values = list(location = 'C.12580', year = '2020'),
                                                from.ontology.names = 'age.ont',
                                                restratify.age = T, desired.age.brackets = c('13-29 years', '30-44 years'))
check("no metadata attributes when none are requested",
      length(intersect(names(attributes(restratified.no.attributes)), c('url', 'details', 'metadata.ids', 'metadata'))), 0L)

#-- 8. Unhashing a stored url array leaves missing cells NA (stored arrays mark them NaN) --#
stored.url = dm$url$new$estimate$cdc$ont$location__year__sex
stored.url[1] = NaN
unhashed = dm$unhash.url(stored.url)
check("a NaN cell unhashes to NA", is.na(unhashed[1]), TRUE)
check("the other cells still unhash", all(grepl("^http", unhashed[-1][!is.na(stored.url[-1])])) && any(!is.na(stored.url[-1])), TRUE)
check("unhashing keeps the shape of the array", dim(unhashed), dim(stored.url))

cat("All metadata aggregation tests passed\n")
