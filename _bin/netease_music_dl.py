#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os
import sys
import requests
import urllib.request


URL_PATTERN_ALBUM = 'http://music.163.com/api/album/{}'
URL_PATTERN_PLAYLIST = 'http://music.163.com/api/playlist/detail?id={}'
HEADERS = {
    'User-Agent': 'Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US; rv:1.9.1.6) Gecko/20091201 Firefox/3.5.6',
    'Referer': 'http://music.163.com/'
}
ALBUM_GET_PARAM = {
    'id': '',
    'csrf_token': ''
}


def get_playlist(playlist_id):
    url = URL_PATTERN_PLAYLIST.format(playlist_id)
    resp = requests.get(url)
    data = resp.json()
    return data['result']


def get_album(album_id):
    url = URL_PATTERN_ALBUM.format(album_id)
    resp = requests.get(url, params=ALBUM_GET_PARAM, headers=HEADERS)
    return resp.json()


def save_track(track, folder, position):
    name = track['name'].strip()
    pos = '{:02d}'.format(position)
    fname = pos + ' - ' + name + '.mp3'
    fname = fname.replace('/', '_').strip()
    fpath = os.path.abspath(os.path.join(folder, fname))
    if os.path.exists(fpath):
        return
    print("Downloading", fpath, "...")

    resp = urllib.request.urlopen(track['mp3Url'])
    data = resp.read()
    resp.close()

    with open(fpath, 'wb') as mp3:
        mp3.write(data)


def download_playlist(playlist_id, folder='.'):
    playlist = get_playlist(playlist_id)

    name = playlist_id + '_' + playlist['name']
    folder = os.path.join(folder, name)
    os.makedirs(folder)

    for idx, track in enumerate(playlist['tracks']):
        save_track(track, folder, idx + 1)


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("usage: {} <playlist id>".format(sys.argv[0]))
        sys.exit(1)
    else:
        download_playlist(sys.argv[1])
