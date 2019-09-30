#!/usr/bin/env python3

import requests, csv, itertools, multiprocessing as mp, os.path

def get_keys():
    '''Assemble all the year/context/drug keys'''
    years = [str(x) for x in range(1997, 2015 + 1)]
    tocs = ['AC', 'HC', 'ACHC']

    with open('abx_keys.tsv') as f:
        reader = csv.DictReader(f, dialect='excel-tab')
        abxs = [row['key'] for row in reader]

    return list(itertools.product(years, tocs, abxs))

def get_data(key):
    '''Download the html page from ECDC'''
    year, toc, abx = key
    payload = {'Year': year, 'Antimicrobial': abx, 'TypeOfCare': toc}
    r = requests.post('https://ecdc.europa.eu/en/antimicrobial-consumption/database/rates-country', data=payload)
    return r

def data_fn(key):
    '''Filename corresponding to a key'''
    # escape the forward slash the 'HIV/AIDS' keys
    escaped_key = '_'.join(key).translate(str.maketrans('/', '-'))
    return os.path.join('esac-html', escaped_key + '.html')

def write_data(key):
    '''Request the data associated with the key and write it to the appropriate file'''
    print(key)
    r = get_data(key)
    html = r.text

    fn = data_fn(key)
    with open(fn, 'w') as f:
        f.write(html)

    return True


if __name__ == '__main__':
    keys = get_keys()

    # find the keys that haven't been written yet
    to_do_keys = [k for k in keys if not os.path.isfile(data_fn(k))]
    print('working on {} of {} total keys'.format(len(to_do_keys), len(keys)))

    # create 20 processes to download each key
    with mp.Pool(20) as p:
        p.map(write_data, to_do_keys)
