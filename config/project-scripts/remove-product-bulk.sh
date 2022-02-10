#!/bin/bash
while read p; do
  /project/config/project-scripts/remove-product.sh "$p"
done < /project/config/project-scripts/products.txt
