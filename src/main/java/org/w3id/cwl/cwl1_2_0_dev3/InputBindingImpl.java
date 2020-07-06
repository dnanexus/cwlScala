package org.w3id.cwl.cwl1_2_0_dev3;

import org.w3id.cwl.cwl1_2_0_dev3.utils.LoaderInstances;
import org.w3id.cwl.cwl1_2_0_dev3.utils.LoadingOptions;
import org.w3id.cwl.cwl1_2_0_dev3.utils.LoadingOptionsBuilder;
import org.w3id.cwl.cwl1_2_0_dev3.utils.SavableImpl;
import org.w3id.cwl.cwl1_2_0_dev3.utils.ValidationException;

/** Auto-generated class implementation for <I>https://w3id.org/cwl/cwl#InputBinding</I><br> */
public class InputBindingImpl extends SavableImpl implements InputBinding {
  private LoadingOptions loadingOptions_ = new LoadingOptionsBuilder().build();
  private java.util.Map<String, Object> extensionFields_ = new java.util.HashMap<String, Object>();

  private java.util.Optional<Boolean> loadContents;

  /**
   * Getter for property <I>https://w3id.org/cwl/cwl#InputBinding/loadContents</I><br>
   *
   * <BLOCKQUOTE>
   *
   * Use of `loadContents` in `InputBinding` is deprecated. Preserved for v1.0 backwards
   * compatability. Will be removed in CWL v2.0. Use `InputParameter.loadContents` instead.
   *
   * <p>Only valid when `type: File` or is an array of `items: File`.
   *
   * <p>Read up to the first 64 KiB of text from the file and place it in the "contents" field of
   * the file object for use by expressions. *
   *
   * </BLOCKQUOTE>
   */
  public java.util.Optional<Boolean> getLoadContents() {
    return this.loadContents;
  }

  /**
   * Used by {@link org.w3id.cwl.cwl1_2_0_dev3.utils.RootLoader} to construct instances of
   * InputBindingImpl.
   *
   * @param __doc_ Document fragment to load this record object from (presumably a {@link
   *     java.util.Map}).
   * @param __baseUri_ Base URI to generate child document IDs against.
   * @param __loadingOptions Context for loading URIs and populating objects.
   * @param __docRoot_ ID at this position in the document (if available) (maybe?)
   * @throws ValidationException If the document fragment is not a {@link java.util.Map} or
   *     validation of fields fails.
   */
  public InputBindingImpl(
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
      throw new ValidationException("InputBindingImpl called on non-map");
    }
    final java.util.Map<String, Object> __doc = (java.util.Map<String, Object>) __doc_;
    final java.util.List<ValidationException> __errors =
        new java.util.ArrayList<ValidationException>();
    if (__loadingOptions != null) {
      this.loadingOptions_ = __loadingOptions;
    }
    java.util.Optional<Boolean> loadContents;

    if (__doc.containsKey("loadContents")) {
      try {
        loadContents =
            LoaderInstances.optional_BooleanInstance.loadField(
                __doc.get("loadContents"), __baseUri, __loadingOptions);
      } catch (ValidationException e) {
        loadContents = null; // won't be used but prevents compiler from complaining.
        final String __message = "the `loadContents` field is not valid because:";
        __errors.add(new ValidationException(__message, e));
      }

    } else {
      loadContents = null;
    }
    if (!__errors.isEmpty()) {
      throw new ValidationException("Trying 'RecordField'", __errors);
    }
    this.loadContents = (java.util.Optional<Boolean>) loadContents;
  }
}
