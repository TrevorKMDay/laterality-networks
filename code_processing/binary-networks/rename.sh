#!/bin/bash

cd flipped || exit

for i in *.nii ; do
    newname=${i//dtseries/dscalar}
    mv "${i}" "${newname}"
done