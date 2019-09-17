# -*- coding: utf-8 -*-
"""
Created on Mon Jul  8 13:42:19 2019

@author: Eric Born
"""

#import requests
#from bs4 import BeautifulSoup as bs
#
#lst = ['https://sites.google.com/view/gamescience',
#       'https://sites.google.com/view/gamescience/stuff',
#       'https://sites.google.com/view/gamescience/thanks',
#       'https://sites.google.com/view/gamescience/tutorials',
#       'https://sites.google.com/view/gamescience/resume']
#
#for j in range(1, 5):
#    for i in range(len(lst)):
#        page = requests.get(lst[i])
#        pageSrc = page.content.decode('utf-8')


# 1. Execute a Metadata Request
# An application can request columns data by calling the list method on the Analytics service object.
# The method requires an reportType parameter that specifies the column data to retrieve.
# For example, the following code requests columns for the ga report type.

try:
  results = service.metadata().columns().list(reportType='ga').execute()

except TypeError, error:
  # Handle errors in constructing a query.
  print ('There was an error in constructing your query : %s' % error)

except HttpError, error:
  # Handle API errors.
  print ('Arg, there was an API error : %s : %s' %
         (error.resp.status, error._get_reason()))

# 2. Print out the Columns data
# The components of the result can be printed out as follows:

def print_metadata_report(results):
  print 'Metadata Response Report'
  print_report_info(results)
  print_attributes(results.get('attributeNames'))
  print_columns(results)


def print_report_info(columns):
  print "Metadata Report Info"
  if columns:
    print 'Kind           = %s' % columns.get('kind')
    print 'Etag           = %s' % columns.get('etag')
    print 'Total Results  = %s' % columns.get('totalResults')


def print_attributes(attributes):
  if attributes:
    print 'Attribute Names:'
    for attribute in attributes:
      print attribute

def print_columns(columns_data):
  if columns_data:
    print 'Columns:'

    columns = columns_data.get('items', [])

    for column in columns:
      print
      print '%15s = %35s' % ('Column ID', column.get('id'))
      print '%15s = %35s' % ('Kind', column.get('kind'))

      column_attributes = column.get('attributes', [])

      for name, value in column_attributes.iteritems():
        print '%15s = %35s' % (name, value)