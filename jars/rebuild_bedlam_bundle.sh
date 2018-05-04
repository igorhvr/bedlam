#!/bin/bash
pushd . && cd /base/bedlam && rm jars/bedlam-bundle.jar &&  zip -0 -r jars/bedlam-bundle.jar iasylum && popd

