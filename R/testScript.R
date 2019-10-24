type = "bar"
labels = c('a', 'b', 'c', 'd', 'e')

datasets = list(list(label = "X", data = list(1, 2, 3)), list(label = "Y", data =
                                                                list(2, 4, 6)))
data = list(labels = labels, datasets = datasets)

quickchartR(type, data)

