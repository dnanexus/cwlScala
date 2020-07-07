CWL v1.2.0-dev4 to WDL v1.0 mapping

Author: Michael R. Crusoe on behalf of DNAnexus, Inc.

Copyright: 2020 DNAnexus, Inc.

Date: 2020-07-06

# Introduction

This document maps the concepts from the draft CWL standards v1.2.0-dev4 to the OpenWDL v1.0 specification. Implementation hints with regards to the Cromwell and dxWDL engines and their conventions are also provided. If implementation could be simplified by using part of the CWL reference implementation ("cwltool") for command line rendering and Docker invocation then this is indicated along with non-cwltool-based suggestions.

# Sources

https://www.commonwl.org/v1.2.0-dev4/CommandLineTool.html

https://www.commonwl.org/v1.2.0-dev4/Workflow.html

https://github.com/openwdl/wdl/blob/9049a884b56aebb84bce5b9a164e84584cc573ac/versions/1.0/SPEC.md (2020-06-23)

https://cromwell.readthedocs.io/en/stable/RuntimeAttributes/

https://github.com/dnanexus/dxWDL/blob/41f7ee24961fdd782a519f3459d06c7780376f3b/doc/ExpertOptions.md#extensions (2020-06-23)

https://github.com/openwdl/wdl/blob/main/versions/development/SPEC.md

# CWL to WDL type mappings

`'null'`: in type definitions can be mapped to "`?`" or “`+`” (optionality) in WDL. In user objects it maps to WDL’s `null`.

`boolean`: -> WDL `Boolean`.

`float`: WDL `Float`.

`int`: WDL `Int`.

`long`: WDL `Int`.

`double`: WDL `Float`.

`string`: WDL `String`.

`File` -> WDL `File` except that there is no direct analogue in WDL for most of the CWL `File` properties: `dirname`, `nameroot`, `nameext`, `checksum`, `secondaryFiles`, `format`, or `contents`. CWL `File.basename` property -> WDL `basename(File)` function. CWL’s `File.size` property -> WDL `size(File)` function.

`Directory`: no WDL v1.0 analogue.

<a href="https://www.commonwl.org/v1.2.0-dev4/CommandLineTool.html#CommandInputArraySchema">`CommandInputArraySchema`</a> -> WDL `Array`, though CWL arrays can have multiple item types and it appears that WDL arrays cannot. The CWL parameter reference `$(array.length)` is equivalent to WDL the `length(array)` function call.

The `inputBinding` for a CWL array can be rewritten as part of the command line rendering, but it must not be lost.

<a href="https://www.commonwl.org/v1.2.0-dev4/CommandLineTool.html#CommandInputEnumSchema">CommandInputEnumSchema</a> does not have a WDL analogue, but could be reduced to a WDL `String` if you want to throw away the value constraint and GUI hint. For dxWDL the choice constraint can be represented by <a href="https://github.com/dnanexus/dxWDL/blob/master/doc/ExpertOptions.md#parameter_meta-section">`parameter_meta.choices`</a>.

<a href="https://www.commonwl.org/v1.2.0-dev4/CommandLineTool.html#CommandInputRecordSchema">CommandInputRecordSchema</a> -> <a href="https://github.com/openwdl/wdl/blob/9049a884b56aebb84bce5b9a164e84584cc573ac/versions/1.0/SPEC.md#struct-definition">WDL Struct</a>

"`?`" type postfix: WDL "`?`" type postfix.

# Document metadata

CWL documents allow unlimited metadata using user-referenced vocabularies. <a href="https://www.commonwl.org/v1.2.0-dev4/Workflow.html#Extensions_and_metadata">For generic metadata the schema.org ontology is recommended by the CWL standards</a>.

A mapping of schema.org annotations that are commonly found in CWL documents to dxWDL `meta.details` could be written.

# CWL common Process attributes

Here are elements common to CWL `Workflow`, `CommandLineTool`, and `ExpressionTool`s:

`id`: the string after the WDL keyword `workflow` or `task` (the workflow/task `name` in the WDL Hermes grammar).

`label` and `doc`: could go into WDL’s <a href="https://github.com/openwdl/wdl/blob/main/versions/1.0/SPEC.md#metadata-section">`meta`</a>data section. For <a href="https://github.com/dnanexus/dxWDL/blob/master/doc/ExpertOptions.md#meta-section">dxWDL</a> `label` would map to `meta.summary` and `doc` to `meta.description`.

`intent`: metadata, can be ignored. Could be represented in WDL under `meta.intent` (but that is not a standardized key for WDL v1.0, dxWDL, Cromwell, nor the proposed WDL 2.0).

# CWL <a href="https://www.commonwl.org/v1.2.0-dev4/CommandLineTool.html#CommandLineTool">`CommandLineTool`</a>

-> <a href="https://github.com/openwdl/wdl/blob/main/versions/1.0/SPEC.md#task-definition">WDL `task`</a>

`class`: always `CommandLineTool`; not needed for WDL.

`CommandLineTool.hints`: see `CommandLineTool.requirements` below.

`baseCommand`: the first entry/entries in the WDL `command` section.

`arguments`: an array of entries to be added to the WDL `command` section. Be careful, the result of `inputBinding`s from the inputs section may insert themselves into this list. See the `inputBinding` section below for more details.

`stdin`: indicates the path of a file to pipe into the tool being described. No direct equivalent in WDL v1.0. If not using cwltool for execution then `stdin`’ could be represented in the WDL command section by adding `< ` and the value of this field to the end of the main command block.

`stdout` and `stderr`: sets the name of the stdout/stderr files; similar to WDL’s `stdout()` and `stderr()` functions. See the `outputBinding` section for more information.

`successCodes`: no WDL v1.0 equivalent but can be mapped to Cromwell’s usage of `runtime.`<a href="https://cromwell.readthedocs.io/en/stable/RuntimeAttributes/#continueonreturncode">`continueOnReturnCode`</a> . If not using cwltool for execution, can also be expressed as an addition to the WDL command section (basically: capture the exit code with `$?` and if it isn’t one of the `successCodes` then exit non-zero).

`temporaryFailCodes` and `permanentFailCodes`: no WDL v1.0 equivalent. Like `successCodes` they could be expressed as an addition to the WDL `command` section, if cwltool is not used for execution.

## `CommandLineTool.`<a href="https://www.commonwl.org/v1.2.0-dev4/CommandLineTool.html#CommandInputParameter">`inputs`</a>

-> <a href="https://github.com/openwdl/wdl/blob/main/versions/1.0/SPEC.md#task-input-declaration">WDL's `task` input declaration</a>

`id`: the name after the WDL type identifier (the type `name` in the WDL Hermes grammar).

`label` and `doc`: could become `help` values in WDL’s <a href="https://github.com/openwdl/wdl/blob/main/versions/1.0/SPEC.md#parameter-metadata-section">`parameter_meta`</a>data section. In a dxWDL context `doc` can be mapped to `parameter_meta.id.description` and `label` to `parameter_meta.id.label`.

`streamable`, `format`, `loadContents`, `loadListing`: appears to have no WDL v1.0 analogues.

`streamable: true` can be mapped to dxWDL’s <a href="https://github.com/dnanexus/dxWDL/blob/master/doc/ExpertOptions.md#streaming">`stream`</a> hint via `parameter_meta`.

`format`: here the user provided vocabulary can be referenced to discover common filename extensions for recording in dxWDL style `parameter_meta.id.patterns`. 

`default`: ‘default’ in WDL’s Expression Placeholder Options or as the expression in a WDL input declaration.

### `CommandLineTool.`<a href="https://www.commonwl.org/v1.2.0-dev4/CommandLineTool.html#CommandInputParameter">`inputs`</a>`.inputBinding`

(a.k.a <a href="https://www.commonwl.org/v1.2.0-dev4/CommandLineTool.html#CommandLineBinding">CommandLineBinding</a> ; not relevant if using cwltool or other CWL aware code to render the command line)

`position`: not a concept in WDL, but can be used to order the resultant items in the WDL `command` section.

`valueFrom`: if not present, then use the <a href="https://www.commonwl.org/v1.2.0-dev4/CommandLineTool.html#CommandLineBinding">chart</a> to represent the linked input textually. If present, then evaluate any CWL expressions embedded and use the result in conjunction with the following modifiers

`prefix` & `separate`: if `separate` is `true` (or missing) then the `prefix` goes directly as a standalone item in WDL command section prior to the result of the `valueFrom`. If `separate` is `false` then the prefix is prepended to the result of the `valueFrom`.

`itemSeparator` -> WDL’s ‘<a href="https://github.com/openwdl/wdl/blob/main/versions/1.0/SPEC.md#sep">`sep`</a>’

`shellQuote: false` with `ShellCommandRequirement` in the requirements section allows for shell metacharacters to survive unquoted. If this is absent, then shell-quote the result of the remaining CommandLineBinding elements.

<a href="https://www.commonwl.org/v1.2.0-dev4/CommandLineTool.html#SecondaryFileSchema">`secondaryFile`</a>s: additional WDL File input(s), but it has no identifier/name (so generate a random identifier/name if need be), stored in the same directory as the main File. Note that the CWL compact form uses a `?` suffix on the pattern to indicate optionality.

## `CommandLineTool.`<a href="https://www.commonwl.org/v1.2.0-dev4/CommandLineTool.html#CommandOutputParameter">`outputs`</a>

-> https://github.com/openwdl/wdl/blob/main/versions/1.0/SPEC.md#outputs-section

`CommandOutputParameter.id`: the name after the WDL type identifier (the type `name` in the Hermes grammar).

See the inputs section for a discussion about `CommanLineTool.outputs.id.{label,secondaryFiles,streamable,doc,format}`.

### `ComandLineTool.outputs.id.`<a href="https://www.commonwl.org/v1.2.0-dev4/CommandLineTool.html#CommandOutputBinding">`outputBinding`</a>

`outputBinding.glob`: WDL `glob()` ; note that CWL does not require that `bash` be a part of the Docker format software container. In CWL, `glob` is defined as "using POSIX glob(3) pathname matching", so this may require changes to your execution scripts if not using cwltool or other CWL aware code.

`outputBinding.{outputEval,loadListing,loadContest}`: no direct WDL analogues. It may be possible to match some CWL Expressions to WDL’s `read_int()`, `read_lines()`, `read_float()`, `read_boolean()`. No implementation needed if using cwltool.

In this `outputs` section only, two more CWL types are allowed (as a shortcut). `stdout` corresponding to WDL’s `stdout()` function. And likewise for `stderr` and WDL’s `stderr()` function. If the stdout/stdin names have been set in the CommandLineTool definition then the resulting CWL File objects will inherit those values as the `basename`. Otherwise the `basename` is random.

## `CommandLineTool.requirements`

### <a href="https://www.commonwl.org/v1.2.0-dev4/CommandLineTool.html#InlineJavascriptRequirement">`InlineJavascriptRequirement`</a> 

required for CWL Expressions (but not required for CWL Parameter References). No WDL analogue.

### <a href="https://www.commonwl.org/v1.2.0-dev4/CommandLineTool.html#SchemaDefRequirement">`SchemaDefRequirement`</a>

defines custom named array, enum, or record types; see the above section on CWL to WDL type mapping.

### <a href="https://www.commonwl.org/v1.2.0-dev4/CommandLineTool.html#DockerRequirement">`DockerRequirement`</a>

`dockerPull` or `dockerImageId`: WDL’s `runtime.docker`.

The remaining elements (`dockerLoad`, `dockerFile`, `dockerImport`, `dockerOutputDirectory`) do not have WDL analogues. In a dxWDL context, the image referenced by `dockerLoad`/`dockerImport` could be uploaded to DNAnexus and <a href="https://github.com/dnanexus/dxWDL/blob/41f7ee24961fdd782a519f3459d06c7780376f3b/doc/ExpertOptions.md#storing-a-docker-image-as-a-file">referenced via a dx:// URI</a>. 

Likewise a `dockerFile` could be built and also uploaded. 

If cwltool is not used then `dockerOutputDirectory` can be realized by ensuring that the specified directory is mounted from a writable path outside the container that has as much space available as required by `ResourceRequirements.outdirMin`.

### <a href="https://www.commonwl.org/v1.2.0-dev4/CommandLineTool.html#SoftwareRequirement">`SoftwareRequirement`</a>

Can be turned into a WDL `runtime.docker` by using http://biocontainers.pro or http://bio.tools to do a lookup. Almost always accompanied by a `DockerRequirement`, so it can be ignored. However it can be used to generate <a href="https://github.com/dnanexus/dxWDL/blob/master/doc/ExpertOptions.md#meta-section">dxWDL style entries</a> in `meta.details.citations`

### <a href="https://www.commonwl.org/v1.2.0-dev4/CommandLineTool.html#InitialWorkDirRequirement">`InitialWorkDirRequirement`</a>

Specifies how the working directory is to be populated. Possibly synthesizes files on its own or via values from the `inputs` section. No WDL analogue, and may not be needed if using cwltool or another CWL aware codebase.

### <a href="https://www.commonwl.org/v1.2.0-dev4/CommandLineTool.html#EnvVarRequirement">`EnvVarRequirement`</a>

No equivalent in WDL v1.0 to specify custom environment variables. If cwltool is not used, then this can be implemented by adding `export ${envName}=${envValue}` to the beginning of the WDL `command` section.

### <a href="https://www.commonwl.org/v1.2.0-dev4/CommandLineTool.html#ShellCommandRequirement">`ShellCommandRequirement`</a>

See the entry in `inputBinding`.

### <a href="https://www.commonwl.org/v1.2.0-dev4/CommandLineTool.html#ResourceRequirement">`ResourceRequirement`</a>

`ram{Min,Max}`: WDL’s `runtime.memory` with a `MiB` suffix.

`cores{Min,Max}` and `{tmp,out}dir{Min,Max}`: no WDL v1.0 equivalent. `cores{Min,Max}` can be mapped to Cromwell’s usage of `runtime.cpu`.

`{tmp,out}dir{Min,Max}`: can be mapped to Cromwell’s usage of `runtime.disks` as `local-disk` though the values will need to be converted from MiB to GiB.

### <a href="https://www.commonwl.org/v1.2.0-dev4/CommandLineTool.html#WorkReuse">`WorkReuse`</a>

Signal to allow cached results; no WDL v1.0 equivalent. Can be mapped to <a href="dx_ignore_reuse">dxWDL’s `runtime.dx_ignore_reuse`</a> with the logic flipped.

### <a href="https://www.commonwl.org/v1.2.0-dev4/CommandLineTool.html#NetworkAccess">`NetworkAccess`</a>

No WDL v1.0 equivalent. Can be mapped to <a href="https://github.com/dnanexus/dxWDL/blob/master/doc/ExpertOptions.md#runtime-hints">dxWDL’s `runtime.dx_access`</a> as `{"network": “*”}`.

### <a href="https://www.commonwl.org/v1.2.0-dev4/CommandLineTool.html#InplaceUpdateRequirement">InplaceUpdateRequirement</a>

No WDL equivalent. Means that files are allowed to be modified in place if marked with `writable: true` via `InitialWorkDirRequirement`; therefore they cannot be mounted read-only nor can they be streamed.

### <a href="https://www.commonwl.org/v1.2.0-dev4/CommandLineTool.html#ToolTimeLimit">`ToolTimeLimit`</a>

No WDL v1.0 equivalent. Can be mapped to <a href="https://github.com/dnanexus/dxWDL/blob/master/doc/ExpertOptions.md#runtime-hints">dxWDL’s runtime.dx_timeout</a> as `{"minutes": value/60}`.

# CWL <a href="https://www.commonwl.org/v1.2.0-dev4/Workflow.html#Workflow">`Workflow`</a>

-> <a href="https://github.com/openwdl/wdl/blob/9049a884b56aebb84bce5b9a164e84584cc573ac/versions/1.0/SPEC.md#workflow-definition">WDL `workflow`</a>

`class`: always `Workflow`, not needed for WDL.

`inputs`: See `CommandLineTool.inputs` above, except that there is no `inputBinding`.

`outputs`: See `CommandLineTool.outputs` above, except there is no `outputBinding` but there is `outputSource`, `linkMerge`, and `pickValue`. `outputSource` -> WDL’s output expression where the slash between the CWL step name and the step output is replaced with a period. For `linkMerge` and `pickValue` see the discussion in `WorkflowStep.in`.

<a href="https://www.commonwl.org/v1.2.0-dev4/Workflow.html#Requirements_and_hints">`requirements`</a>: list of additional requirements for this workflow, and it flows down to the `CommandLineTool`s and `ExpressionTool`s within, unless there is an intervening requirement at the step level, or the `CommandLineTool`/`ExpressionTool` has the same class of requirement already.

`hints`: like `requirements` but not required. If a step, sub-`Workflow`, or `CommandLineTool`/`ExpressionTool` has the same class of requirement then the class listed in the workflow `hints` is ignored in that context; otherwise the hint flows down to all sub-`Process`es (unless it isn’t applicable to that type of `Process`).

## <a href="https://www.commonwl.org/v1.2.0-dev4/Workflow.html#WorkflowStep">CWL Workflow `steps`</a>

The `steps` field of a CWL `Workflow`, a list of references to other CWL `CommandLineTool`s, sub-`Workflow`s, and `ExpressionTool`s to be run; where their inputs come from; if they are scattered; and if they are run only conditionally.

`run`: either includes the CWL Process as the value or a URI to it. That URI could be a relative path to the CWL description to include, or a remote HTTP(s) URI to fetch.

`id`: the `task_identifier` from WDL’s `call task_name as task_identifier`.

<a href="https://www.commonwl.org/v1.2.0-dev4/Workflow.html#Requirements_and_hints">`requirements`</a>: list of additional requirements for the underlying CWL Process. If the Process already has the same type of requirements under "hints" it will be replaced with this one.

`when`: WDL `if result_of_when_expression then call cwl_workflow_step_id`. Note: the CWL Expression can include references to any of the inputs defined in the `in` section as `$(inputs.id)`.

`scatter`: here is some pseudo WDL code

```wdl

scatter(cwl_scatter_list[0] as scatter0, .. cwl_scatter_list(N) as scatterN) {

  call step_id { input: non_scattered_inputs_spec, cwl_scatter_list[0] = scatter0, .. 

                         cwl_scatter_list[N] = scatterN}}

```

where `non_scattered_inputs_spec` is replaced with the regular variable mappings that don’t involve scattering.

<a href="https://www.commonwl.org/v1.2.0-dev4/Workflow.html#WorkflowStep">`scatterMethod`</a>, for `dotProduct`, see the plain scatter example above

`scatterMethod` = `nested_crossproduct` then produce cartesian cross products with no flattening. I’m not sure if WDL has this capability as zip only works with two arrays.

`scatterMethod` = `flat_crossproduct` is the same as `nested_crossproduct` but then flattened.

`out`: a list of output specified in the underlying Process from the `run` field to make available to other steps. Can be used as an engine optimization; if an output isn’t listed here it can be discarded (or only made available on demand by the user later).

`label` / `doc`: while there is some place for this in WDL task meta data, WDL doesn’t have a place for step level metadata, though this could be added as `Workflow.meta.step_id.{label,description}` as non-standardized metadata.

### <a href="https://www.commonwl.org/v1.2.0-dev4/Workflow.html#WorkflowStepInput">CWL `Workflow` Step input mapping</a>

The `in` field of CWL `Workflow.steps.id`: maps the inputs of the CWL Process in the `run` field to both CWL `Workflow` level inputs and the outputs of other CWL steps.

`id`: the identifier we want to provide a value for. Almost always it is an identifier from the underlying Process from the `run` field, but sometimes it isn’t so we can bring a value into scope for other reasons.

`source`: either a workflow level input id, or `step_id/output_id` -> WDL `task_identifier.output_id`.

A simple example:

``` cwl

class: Workflow
cwlVersion: v1.0
id: my_workflow

inputs: 
  one: string

steps:
   first:
      run: first.cwl
      in:
         { id: input1, source: one }
      out: [result]
   second:
      run: second.cwl
      in: {id: input2: source: first/result }
      out: []
outputs: []
```

``` wdl
version 1.0

import first.wdl as first
import second.wdl as second

workflow my_workflow {
  input { string one }

  call first { input1=one }
  call second { input2=first.result }
}
```

`linkMerge`: if `merge_flattened` then equivalent to WDL `Array[type_of_destination] self = flatten([source])`. If `merge_nested` then equivalent to WDL `Array[type_of_destination] self = [ source[0], .. source[N])`.

`pickValue`: pickValue is evaluated

  1. Once all source values from upstream step or parameters are available.
  2. After linkMerge.
  3. Before scatter or valueFrom.

If `pickValue = first_non_null` then WDL `target_type self = select_first(self)`.

If `pickValue = the_only_non_null` then WDL `target_type self = if length(select_all(self)) != 1 then null else select_first(self)` but really should fail instead of null.

If `pickValue = all_non_null` then WDL `target_type self = select_all(self)`.

`default`: "The default value for this parameter to use if either there is no ‘source’ field, or the value produced by the ‘source’ is null. The default must be applied prior to scattering or evaluating ‘valueFrom’."

`valueFrom`: either a CWL expression to evaluate in the context specified in the standard, or a constant string. If a constant string, then it is equivalent to the WDL `String self = valueFrom_value`.

`loadContents`: makes the first 64KiB of contents of the specified File object available to ‘valueFrom’. Similar to WDL `String self_contents = read_string(source)` if that could be safely limited to the first 64KiB.
