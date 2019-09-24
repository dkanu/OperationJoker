from oauth2client.service_account import ServiceAccountCredentials
from googleapiclient.discovery import build
from httplib2 import Http
from contextlib import closing
import json
import argparse

# Call the API
scopes = ['https://www.googleapis.com/auth/doubleclickbidmanager']
credentials = ServiceAccountCredentials.from_json_keyfile_name('key.json', scopes=scopes)
http_auth = credentials.authorize(Http())
dbm = build('doubleclickbidmanager', 'v1', http=http_auth)

# Parser
parser = argparse.ArgumentParser(
 description='Create a report from a json file.'
)

parser.add_argument(
 '-s',
 '--json_query_source',
 metavar='',
 type=str,
 help='The name of json file containing query resource',
 required=True
)


def create_query_json(json_query_source):
    with closing(open(json_query_source)) as file:
        body = json.load(file)
    dbm.queries().createquery(body=body).execute()
    print("Query created from %s", json_query_source)


args = parser.parse_args()

if __name__ == '__main__':
    create_query_json(args.json_query_source)
