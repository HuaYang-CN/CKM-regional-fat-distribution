# GitHub → Zenodo release checklist

1. Create a new GitHub repository.
2. Upload the contents of this folder, **not the outer ZIP itself**.
3. Confirm `data/private/china/` contains only README/schema files.
4. Confirm no `results/` or downloaded NHANES raw files are committed.
5. Add a repository description and manuscript title.
6. Choose a code license with all authors/institution as appropriate.
7. Create a GitHub release, e.g. `v1.0.0`.
8. Connect the GitHub repository to Zenodo and archive the release.
9. Copy the Zenodo DOI into:
   - `README.md`
   - `CODE_AVAILABILITY_TEXT.txt`
   - the manuscript Code availability statement.
10. Do not change the archived release after submission; create a new version if code changes.

Before uploading, search the repository for:
- investigator-specific absolute home-directory paths
- passwords/tokens
- participant identifiers or clinical datasets
- unpublished confidential files
