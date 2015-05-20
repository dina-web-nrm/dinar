# -*- coding: utf-8 -*-
"""
Convert a "namespace-cleaned" redlist info XML-file to csv utf8
@author: markus
"""
import xml.etree.cElementTree as cElementTree

RLI_XML = "rli.xml"
RLI_CSV = "rli.csv"
global count
count = 0

print "Starting to convert " + RLI_XML + " to " + RLI_CSV
fo = open(RLI_CSV, "w")
fo.write("row_id,status_abbrev,dyntaxa_id\n")
for event, elem in cElementTree.iterparse(RLI_XML):
  if elem.tag == "WebSpeciesFact":
    count += 1    
    red = elem.findtext("FieldValue4") or ""
    tid = elem.findtext("TaxonId") or ""    
    row = str(count) + "," + red + "," + tid + "\n"
    fo.write(row.encode("utf8"))
fo.close()
print "Done."
