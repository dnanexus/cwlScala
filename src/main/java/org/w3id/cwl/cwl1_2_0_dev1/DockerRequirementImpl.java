package org.w3id.cwl.cwl1_2_0_dev1;

import org.w3id.cwl.cwl1_2_0_dev1.utils.LoaderInstances;
import org.w3id.cwl.cwl1_2_0_dev1.utils.LoadingOptions;
import org.w3id.cwl.cwl1_2_0_dev1.utils.LoadingOptionsBuilder;
import org.w3id.cwl.cwl1_2_0_dev1.utils.SavableImpl;
import org.w3id.cwl.cwl1_2_0_dev1.utils.ValidationException;

/**
 * Auto-generated class implementation for <I>https://w3id.org/cwl/cwl#DockerRequirement</I><br>
 *
 * <BLOCKQUOTE>
 *
 * Indicates that a workflow component should be run in a [Docker](http://docker.com) or
 * Docker-compatible (such as [Singularity](https://www.sylabs.io/) and
 * [udocker](https://github.com/indigo-dc/udocker)) container environment and specifies how to fetch
 * or build the image.
 *
 * <p>If a CommandLineTool lists `DockerRequirement` under `hints` (or `requirements`), it may (or
 * must) be run in the specified Docker container.
 *
 * <p>The platform must first acquire or install the correct Docker image as specified by
 * `dockerPull`, `dockerImport`, `dockerLoad` or `dockerFile`.
 *
 * <p>The platform must execute the tool in the container using `docker run` with the appropriate
 * Docker image and tool command line.
 *
 * <p>The workflow platform may provide input files and the designated output directory through the
 * use of volume bind mounts. The platform should rewrite file paths in the input object to
 * correspond to the Docker bind mounted locations. That is, the platform should rewrite values in
 * the parameter context such as `runtime.outdir`, `runtime.tmpdir` and others to be valid paths
 * within the container. The platform must ensure that `runtime.outdir` and `runtime.tmpdir` are
 * distinct directories.
 *
 * <p>When running a tool contained in Docker, the workflow platform must not assume anything about
 * the contents of the Docker container, such as the presence or absence of specific software,
 * except to assume that the generated command line represents a valid command within the runtime
 * environment of the container.
 *
 * <p>A container image may specify an
 * [ENTRYPOINT](https://docs.docker.com/engine/reference/builder/#entrypoint) and/or
 * [CMD](https://docs.docker.com/engine/reference/builder/#cmd). Command line arguments will be
 * appended after all elements of ENTRYPOINT, and will override all elements specified using CMD (in
 * other words, CMD is only used when the CommandLineTool definition produces an empty command
 * line).
 *
 * <p>Use of implicit ENTRYPOINT or CMD are discouraged due to reproducibility concerns of the
 * implicit hidden execution point (For further discussion, see
 * [https://doi.org/10.12688/f1000research.15140.1](https://doi.org/10.12688/f1000research.15140.1)).
 * Portable CommandLineTool wrappers in which use of a container is optional must not rely on
 * ENTRYPOINT or CMD. CommandLineTools which do rely on ENTRYPOINT or CMD must list
 * `DockerRequirement` in the `requirements` section.
 *
 * <p>## Interaction with other requirements
 *
 * <p>If [EnvVarRequirement](#EnvVarRequirement) is specified alongside a DockerRequirement, the
 * environment variables must be provided to Docker using `--env` or `--env-file` and interact with
 * the container's preexisting environment as defined by Docker.
 *
 * </BLOCKQUOTE>
 */
public class DockerRequirementImpl extends SavableImpl implements DockerRequirement {
  private LoadingOptions loadingOptions_ = new LoadingOptionsBuilder().build();
  private java.util.Map<String, Object> extensionFields_ = new java.util.HashMap<String, Object>();

  private String class_;

  /**
   * Getter for property <I>https://w3id.org/cwl/cwl#DockerRequirement/class</I><br>
   *
   * <BLOCKQUOTE>
   *
   * Always 'DockerRequirement' *
   *
   * </BLOCKQUOTE>
   */
  public String getClass_() {
    return this.class_;
  }

  private java.util.Optional<String> dockerPull;

  /**
   * Getter for property <I>https://w3id.org/cwl/cwl#DockerRequirement/dockerPull</I><br>
   *
   * <BLOCKQUOTE>
   *
   * Specify a Docker image to retrieve using `docker pull`. Can contain the immutable digest to
   * ensure an exact container is used: `dockerPull:
   * ubuntu@sha256:45b23dee08af5e43a7fea6c4cf9c25ccf269ee113168c19722f87876677c5cb2` *
   *
   * </BLOCKQUOTE>
   */
  public java.util.Optional<String> getDockerPull() {
    return this.dockerPull;
  }

  private java.util.Optional<String> dockerLoad;

  /**
   * Getter for property <I>https://w3id.org/cwl/cwl#DockerRequirement/dockerLoad</I><br>
   *
   * <BLOCKQUOTE>
   *
   * Specify a HTTP URL from which to download a Docker image using `docker load`. *
   *
   * </BLOCKQUOTE>
   */
  public java.util.Optional<String> getDockerLoad() {
    return this.dockerLoad;
  }

  private java.util.Optional<String> dockerFile;

  /**
   * Getter for property <I>https://w3id.org/cwl/cwl#DockerRequirement/dockerFile</I><br>
   *
   * <BLOCKQUOTE>
   *
   * Supply the contents of a Dockerfile which will be built using `docker build`. *
   *
   * </BLOCKQUOTE>
   */
  public java.util.Optional<String> getDockerFile() {
    return this.dockerFile;
  }

  private java.util.Optional<String> dockerImport;

  /**
   * Getter for property <I>https://w3id.org/cwl/cwl#DockerRequirement/dockerImport</I><br>
   *
   * <BLOCKQUOTE>
   *
   * Provide HTTP URL to download and gunzip a Docker images using `docker import. *
   *
   * </BLOCKQUOTE>
   */
  public java.util.Optional<String> getDockerImport() {
    return this.dockerImport;
  }

  private java.util.Optional<String> dockerImageId;

  /**
   * Getter for property <I>https://w3id.org/cwl/cwl#DockerRequirement/dockerImageId</I><br>
   *
   * <BLOCKQUOTE>
   *
   * The image id that will be used for `docker run`. May be a human-readable image name or the
   * image identifier hash. May be skipped if `dockerPull` is specified, in which case the
   * `dockerPull` image id must be used. *
   *
   * </BLOCKQUOTE>
   */
  public java.util.Optional<String> getDockerImageId() {
    return this.dockerImageId;
  }

  private java.util.Optional<String> dockerOutputDirectory;

  /**
   * Getter for property <I>https://w3id.org/cwl/cwl#DockerRequirement/dockerOutputDirectory</I><br>
   *
   * <BLOCKQUOTE>
   *
   * Set the designated output directory to a specific location inside the Docker container. *
   *
   * </BLOCKQUOTE>
   */
  public java.util.Optional<String> getDockerOutputDirectory() {
    return this.dockerOutputDirectory;
  }

  /**
   * Used by {@link org.w3id.cwl.cwl1_2_0_dev1.utils.RootLoader} to construct instances of
   * DockerRequirementImpl.
   *
   * @param __doc_ Document fragment to load this record object from (presumably a {@link
   *     java.util.Map}).
   * @param __baseUri_ Base URI to generate child document IDs against.
   * @param __loadingOptions Context for loading URIs and populating objects.
   * @param __docRoot_ ID at this position in the document (if available) (maybe?)
   * @throws ValidationException If the document fragment is not a {@link java.util.Map} or
   *     validation of fields fails.
   */
  public DockerRequirementImpl(
      final Object __doc_,
      final String __baseUri_,
      LoadingOptions __loadingOptions,
      final String __docRoot_) {
    super(__doc_, __baseUri_, __loadingOptions, __docRoot_);
    // Prefix plumbing variables with '__' to reduce likelihood of collision with
    // generated names.
    String __baseUri = __baseUri_;
    String __docRoot = __docRoot_;
    if (!(__doc_ instanceof java.util.Map)) {
      throw new ValidationException("DockerRequirementImpl called on non-map");
    }
    final java.util.Map<String, Object> __doc = (java.util.Map<String, Object>) __doc_;
    final java.util.List<ValidationException> __errors =
        new java.util.ArrayList<ValidationException>();
    if (__loadingOptions != null) {
      this.loadingOptions_ = __loadingOptions;
    }
    String class_;
    try {
      class_ =
          LoaderInstances.uri_StringInstance_False_True_None.loadField(
              __doc.get("class"), __baseUri, __loadingOptions);
    } catch (ValidationException e) {
      class_ = null; // won't be used but prevents compiler from complaining.
      final String __message = "the `class` field is not valid because:";
      __errors.add(new ValidationException(__message, e));
    }
    java.util.Optional<String> dockerPull;

    if (__doc.containsKey("dockerPull")) {
      try {
        dockerPull =
            LoaderInstances.optional_StringInstance.loadField(
                __doc.get("dockerPull"), __baseUri, __loadingOptions);
      } catch (ValidationException e) {
        dockerPull = null; // won't be used but prevents compiler from complaining.
        final String __message = "the `dockerPull` field is not valid because:";
        __errors.add(new ValidationException(__message, e));
      }

    } else {
      dockerPull = null;
    }
    java.util.Optional<String> dockerLoad;

    if (__doc.containsKey("dockerLoad")) {
      try {
        dockerLoad =
            LoaderInstances.optional_StringInstance.loadField(
                __doc.get("dockerLoad"), __baseUri, __loadingOptions);
      } catch (ValidationException e) {
        dockerLoad = null; // won't be used but prevents compiler from complaining.
        final String __message = "the `dockerLoad` field is not valid because:";
        __errors.add(new ValidationException(__message, e));
      }

    } else {
      dockerLoad = null;
    }
    java.util.Optional<String> dockerFile;

    if (__doc.containsKey("dockerFile")) {
      try {
        dockerFile =
            LoaderInstances.optional_StringInstance.loadField(
                __doc.get("dockerFile"), __baseUri, __loadingOptions);
      } catch (ValidationException e) {
        dockerFile = null; // won't be used but prevents compiler from complaining.
        final String __message = "the `dockerFile` field is not valid because:";
        __errors.add(new ValidationException(__message, e));
      }

    } else {
      dockerFile = null;
    }
    java.util.Optional<String> dockerImport;

    if (__doc.containsKey("dockerImport")) {
      try {
        dockerImport =
            LoaderInstances.optional_StringInstance.loadField(
                __doc.get("dockerImport"), __baseUri, __loadingOptions);
      } catch (ValidationException e) {
        dockerImport = null; // won't be used but prevents compiler from complaining.
        final String __message = "the `dockerImport` field is not valid because:";
        __errors.add(new ValidationException(__message, e));
      }

    } else {
      dockerImport = null;
    }
    java.util.Optional<String> dockerImageId;

    if (__doc.containsKey("dockerImageId")) {
      try {
        dockerImageId =
            LoaderInstances.optional_StringInstance.loadField(
                __doc.get("dockerImageId"), __baseUri, __loadingOptions);
      } catch (ValidationException e) {
        dockerImageId = null; // won't be used but prevents compiler from complaining.
        final String __message = "the `dockerImageId` field is not valid because:";
        __errors.add(new ValidationException(__message, e));
      }

    } else {
      dockerImageId = null;
    }
    java.util.Optional<String> dockerOutputDirectory;

    if (__doc.containsKey("dockerOutputDirectory")) {
      try {
        dockerOutputDirectory =
            LoaderInstances.optional_StringInstance.loadField(
                __doc.get("dockerOutputDirectory"), __baseUri, __loadingOptions);
      } catch (ValidationException e) {
        dockerOutputDirectory = null; // won't be used but prevents compiler from complaining.
        final String __message = "the `dockerOutputDirectory` field is not valid because:";
        __errors.add(new ValidationException(__message, e));
      }

    } else {
      dockerOutputDirectory = null;
    }
    if (!__errors.isEmpty()) {
      throw new ValidationException("Trying 'RecordField'", __errors);
    }
    this.class_ = (String) class_;
    this.dockerPull = (java.util.Optional<String>) dockerPull;
    this.dockerLoad = (java.util.Optional<String>) dockerLoad;
    this.dockerFile = (java.util.Optional<String>) dockerFile;
    this.dockerImport = (java.util.Optional<String>) dockerImport;
    this.dockerImageId = (java.util.Optional<String>) dockerImageId;
    this.dockerOutputDirectory = (java.util.Optional<String>) dockerOutputDirectory;
  }
}
