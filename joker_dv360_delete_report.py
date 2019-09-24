import googleapiclient
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
 description='Delete Queries'
)

parser.add_argument(
 '-d',
 '--delete_queue',
 nargs="+",
 metavar='',
 type=int,
 help='List of queries to be deleted',
 required=True
)


def delete_queries(delete_queue):
    print(delete_queue)

    response = dbm.queries().listqueries().execute()
    query_list = []
    for q in response['queries']:
        query_list.append(int(q['queryId']))

    for q in delete_queue:
        if q in query_list:
            dbm.queries().deletequery(queryId=q).execute()
            print('Query with Id %i deleted', q)
        else:
            print('Query with ID %i not found', q)


args = parser.parse_args()


if __name__ == '__main__':
    delete_queries(args.delete_queue)
