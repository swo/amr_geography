#!/usr/bin/env python3

import bs4, glob, re
import scrape

query_keys = scrape.get_keys()
headers = ['year', 'title_year', 'toc', 'title_toc', 'abx', 'title_abx', 'title_atc', 'country', 'did']

def is_data_table(table):
    # if it has a subtable, no good
    if table.find('table') is not None:
        return False

    # does it have rows, some of which have countries?
    rows = table.find_all('tr')
    fields = [col.get_text() for row in rows for col in row.find_all('td')]
    return 'Austria' in fields

def only_for_which(xs, f):
    '''Return the one x for which f is true'''
    trues = [x for x in xs if f(x)]

    if len(trues) == 1:
        return trues[0]
    else:
        raise ValueError

def extract_title_dict(x):
    m = re.match('Consumption of (?P<title_abx>.+?) \(ATC group (?P<title_atc>.+?)\) in the (?P<title_toc>.+?) in Europe, reporting year (?P<title_year>\d{4})', x)
    return {k: m.group(k) for k in ['title_abx', 'title_atc', 'title_toc', 'title_year']}

def merge_dicts(*dicts):
    out = {}

    for d in dicts:
        out.update(d)

    return out

def print_row(row_dict):
    assert all([x in row_dict for x in headers])
    fields = [row_dict[x] for x in headers]
    print(*fields, sep='\t')

print(*headers, sep='\t')
for query_key in query_keys:
    query_year, query_toc, query_abx = query_key

    fn = scrape.data_fn(query_key)

    with open(fn) as f:
        content = f.read()

    s = bs4.BeautifulSoup(content, 'html.parser')
    tables = s.find_all('table')
    data_table = only_for_which(tables, is_data_table)

    rows = data_table.find_all('tr')

    query_dict = {'year': query_year, 'toc': query_toc, 'abx': query_abx}
    title_dict = None
    header_row = False
    for row in rows:
        cols = row.find_all('td')
        fields = [col.get_text() for col in cols]

        nonempty_fields = [x for x in fields if x != '']
        if len(nonempty_fields) == 0:
            # this is the blank first row, do nothing
            pass
        elif len(nonempty_fields) == 1:
            # this is the title row
            title_dict = extract_title_dict(nonempty_fields[0])

            # the header row is next
            header_row = True
        elif len(nonempty_fields) == 2 and header_row:
            # this is the header row
            assert nonempty_fields == ['Country', 'DDD per 1000 inhabitants and per day']
            header_row = False
        elif len(nonempty_fields) == 2:
            # this is a data row
            assert title_dict is not None
            data_dict = {'country': nonempty_fields[0], 'did': nonempty_fields[1]}
            row_dict = merge_dicts(query_dict, title_dict, data_dict)
            print_row(row_dict)
        else:
            raise RuntimeError()
