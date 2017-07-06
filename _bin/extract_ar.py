#!/usr/bin/env python3

def extract_archive(pathtoarchive, destfolder):

    archive = open(pathtoarchive, 'rb')

    global_header = archive.read(8)
    if global_header != b'!<arch>\n':
        print("{} header: {}".format(pathtoarchive, global_header))
        exit()
    if destfolder[-1] != '/':
        destfolder = destfolder + '/'

    print('Trying to extract object files from ' + pathtoarchive)

    # We don't need the first and second chunk
    # they're just symbol and name tables

    content_descriptor = archive.readline()
    chunk_size = int(content_descriptor[48:57])
    archive.read(chunk_size)

    content_descriptor = archive.readline()
    chunk_size = int(content_descriptor[48:57])
    archive.read(chunk_size)

    unique_key = 0

    while True:

        content_descriptor = archive.readline()

        if len(content_descriptor) < 60:
            break

        chunk_size = int(content_descriptor[48:57])

        output_obj = open(destfolder + pathtoarchive.split('/')[-1] + '.' + str(unique_key) + '.o', 'wb')
        output_obj.write(archive.read(chunk_size))

        if chunk_size%2 == 1:
            archive.read(1)

        output_obj.close()

        unique_key = unique_key + 1

    archive.close()

    print('Object files extracted to ' + destfolder + '.')


if __name__ == "__main__":
    import sys
    if len(sys.argv) < 3:
        print("usage: {} ar_file dest_folder".format(__file__))
        sys.exit(1)
    extract_archive(sys.argv[1], sys.argv[2])
