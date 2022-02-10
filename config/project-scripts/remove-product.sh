#!/bin/bash
USERNAME=${3:-"dba"}
PASSWORD=${4:-"dba"}
TRIPLESTORE=${2:-"triplestore"}
PRODUCT=${1}
if [[ "$#" -le 0 ]]; then
    echo "Usage:"
    echo "   mu script project-scripts remove-product [product-uri]"
    exit -1;
fi

echo "Connecting to $TRIPLESTORE with $USERNAME"
isql-v -H $TRIPLESTORE -U $USERNAME -P $PASSWORD <<EOF
SPARQL
PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
PREFIX gr: <http://purl.org/goodrelations/v1#>
PREFIX dct: <http://purl.org/dc/terms/>
PREFIX stock: <http://data.rollvolet.be/vocabularies/stock-management/>
DELETE {
  GRAPH <http://mu.semte.ch/graphs/rollvolet> {
  <${PRODUCT}> ?pP ?pO .
    ?purchaseOffering ?poP ?poO .
    ?purchasePriceSpec ?ppsP ?ppsO .
    ?salesOffering ?soP ?soO .
    ?salesPriceSpec ?spsP ?spsO .
    ?location ?locP ?locO .
  }
} WHERE {
  GRAPH <http://mu.semte.ch/graphs/rollvolet> {
    <${PRODUCT}> a gr:SomeItems ;
      ext:purchaseOffering ?purchaseOffering ;
      ext:salesOffering ?salesOffering ;
      stock:location ?location ;
    ?pP ?pO .
    ?purchaseOffering gr:hasPriceSpecification ?purchasePriceSpec ; ?poP ?poO .
    ?purchasePriceSpec gr:hasCurrencyValue ?purchasePrice ; ?ppsP ?ppsO .
    ?salesOffering gr:hasPriceSpecification ?salesPriceSpec ; ?soP ?soO .
    ?salesPriceSpec gr:hasCurrencyValue ?salesPrice ; ?spsP ?spsO .
    ?location ?locP ?locO .
  } } ;
exit;
EOF
