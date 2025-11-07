# Security Policy

Tidal is a programming language. Security matters across the entire stack: lexer/parser, type checker, runtime/VM/JIT, and CLI. Please follow this policy when reporting vulnerabilities.

## Supported versions

We aim to fix vulnerabilities on the active `main` branch and in the latest releases. If an issue affects end-of-life versions, we may provide migration guidance instead of a patch release.

## How to report a vulnerability

Please do NOT open public issues.

Preferred private channels:
- GitHub Private Vulnerability Reporting: use the “Report a vulnerability” button on the repository page (if enabled).
- Or contact the maintainers via the contact information in the organization/repository profile.
- If a private channel is unavailable, open a draft Security Advisory or start a Discussion and ask a moderator to move it to a private channel.

When reporting, please include:
- A description of the vulnerability and affected components (lexer/parser/type checker/runtime/VM/JIT/CLI).
- A minimal PoC: a Tidal code snippet or steps to reproduce.
- The versions/commits where the issue is confirmed and your OS/platform.
- An impact assessment and estimated severity (e.g., RCE, DoS, data leak, isolation break, UB, etc.).
- Any temporary mitigations you’re aware of (if any).

If you need an encrypted channel (PGP, etc.), mention this in your first message — we’ll provide keys and instructions.

## What happens next (timelines)

- Acknowledgement of receipt: typically within 3 business days.
- Initial triage and reproduction: 7–14 days.
- Fix development and testing: depends on complexity; for critical issues we strive to ship a fix or an action plan within ~30 days.
- Coordinated responsible disclosure: we’ll agree on an advisory publication date and credit details.

## Disclosure policy

We follow responsible disclosure. Please do not publish details of the vulnerability until a fix is released and/or a publication date is mutually agreed. After the release, we will publish security notes and, if needed, migration guidance.

## Acknowledgements

We appreciate responsible reports and are happy to acknowledge researchers in the release notes/advisory (if you wish).
