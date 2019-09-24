from oauth2client.service_account import ServiceAccountCredentials
from googleapiclient.discovery import build
from httplib2 import Http
import argparse

# Call the API
scopes = ['https://www.googleapis.com/auth/doubleclickbidmanager']
credentials = ServiceAccountCredentials.from_json_keyfile_name('key.json', scopes=scopes)
http_auth = credentials.authorize(Http())
body = {}
dbm = build('doubleclickbidmanager', 'v1', http=http_auth)


# Parser
parser = argparse.ArgumentParser(
 description='List Available Query Ids and Name'
)


def show_queries():
    response = dbm.queries().listqueries().execute()
    print('Id\t\tName')
    if 'queries' in response:
        for q in response['queries']:
            print('%s\t%s' % (q['queryId'], q['metadata']['title']))
    else:
        print('No queries exist')


if __name__ == '__main__':
    show_queries()

