cwlVersion: v1.2
class: CommandLineTool

requirements:
  - class: SchemaDefRequirement
    types:
      - name: name
        type: record
        fields:
          - name: first
            type: string
          - name: last
            type: string
      - name: person
        type: record
        fields:
          - name: name
            type: name
          - name: age
            type: int

inputs:
  - id: person
    type: person

outputs:
  - id: name
    type: string