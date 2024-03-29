#!/usr/bin/env cwl-runner
cwlVersion: v1.2
class: CommandLineTool
id: InputSecondaryFileConformanceTest
doc: |
  Simple test to confirm the implementation of expressions returning a File within a CommandInputParameter.secondaryFile field.

  Use GREP to filter the result from ls to ensure we only get the secondary files in there.

  Related links:
  - Issue: https://github.com/common-workflow-language/cwltool/issues/1232
  - PR: https://github.com/common-workflow-language/cwltool/pull/1233
  - Discourse: https://cwl.discourse.group/t/ask-cwl-to-rename-a-secondary-file/72
requirements:
  InlineJavascriptRequirement: {}
  ShellCommandRequirement: {}
  InitialWorkDirRequirement:
    listing:
    - $(inputs.inputWithSecondary)
hints:
  NetworkAccess:
    networkAccess: true
  LoadListingRequirement:
    loadListing: deep_listing
inputs:
- id: inputWithSecondary
  type: File
  doc: |
    This input will with a secondary file `.accessory`. You could create these files (and its accessory) with:
    ```bash
    touch secondary_file_test.txt
    touch secondary_file_test.txt.accessory
    ```
  secondaryFiles:
  - .accessory
  - |
    ${
      function resolveSecondary(base, secPattern) {
        if (secPattern[0] == '^') {
          var spl = base.split('.');
          var endIndex = spl.length > 1 ? spl.length - 1 : 1;
          return resolveSecondary(spl.slice(undefined, endIndex).join("."), secPattern.slice(1));
        }
        return base + secPattern;
      }
      return [{
          "class": "File",
          "location": self.secondaryFiles[0].location,
          "basename": resolveSecondary(self.basename, '^.accessory')
      }];
    }

baseCommand:
- ls
arguments:
- valueFrom: "|"
  shellQuote: false
  position: 0
- valueFrom: "grep"
  position: 1
- valueFrom: "secondary"
  position: 2

stdout: result
outputs:
- id: output_file
  type: stdout
