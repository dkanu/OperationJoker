from oauth2client.service_account import ServiceAccountCredentials
from googleapiclient.discovery import build
from httplib2 import Http
from contextlib import closing
from urllib.request import urlopen
import argparse

# Call API
scopes = ['https://www.googleapis.com/auth/doubleclickbidmanager']
credentials = ServiceAccountCredentials.from_json_keyfile_name('key.json', scopes=scopes)
http_auth = credentials.authorize(Http())
dbm = build('doubleclickbidmanager', 'v1', http=http_auth)

# Parser
parser = argparse.ArgumentParser(
    description='Download Queries',
    add_help=True
)

parser.add_argument(
 '-g',
 '--query_list',
 nargs="+",
 metavar='',
 type=int,
 help='List of queries to be downloaded',
 required=True
)


def download_report(query_list):

    for q in query_list:
        query = (dbm.queries().getquery(queryId=q).execute())

        try:
            report_url = query['metadata']['googleCloudStoragePathForLatestReport']
            output_file = '%s.csv' % (query['queryId'])

            with open(output_file, 'wb') as output:
                with closing(urlopen(report_url)) as url:
                    output.write(url.read())
            print('Download complete.')
        except ValueError:
            print("Report unavailable, check latestReportRunTimeMs")
            print('LatestReportRunTimeMs: %s' % query['metadata']['latestReportRunTimeMs'])
            print('Report runs Minute of Day: %s' % query['schedule']['nextRunMinuteOfDay'])
            print('Time Zone: %s' % query['schedule']['nextRunTimezoneCode'])


args = parser.parse_args()

if __name__ == '__main__':
    download_report(args.query_list)
