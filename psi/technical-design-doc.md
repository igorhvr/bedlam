# Technical Design: Private Set Intersection Utilities

This design relies on the [OpenMined PSI](https://github.com/OpenMined/PSI) library.
The code is written in Python and requires Python 3.10 due to the available wheel
for `openmined-psi`.

Two command line programs are provided:

- `prepare_intersection.py` – generates the initial PSI message and saves a local
  state file containing the private key needed to continue the protocol.
- `find_intersection.py` – consumes messages from the other party and uses the
  previously stored state to either create the server response or compute the
  final intersection on the client.

## Data Flow
1. **Preparation**
   - Server executes `prepare_intersection.py --role server` with its list of
     strings and the expected size of the client's list.
   - Client executes `prepare_intersection.py --role client` with its list of
     strings.
   - Each command outputs a blob (base64 text) to share with the other party and
     a JSON file with the private key for future steps.
2. **Exchange**
   - Server sends its setup blob to the client.
   - Client sends its request blob to the server.
3. **Server Response**
   - Server runs `find_intersection.py --role server` using its state and the
     client's request blob. The output is a response blob sent back to the
     client.
4. **Client Finalization**
   - Client runs `find_intersection.py --role client` using its state, the
     server setup blob and the response blob. The program writes the intersection
     strings to an output file.

Only the client learns the intersection. To let both parties learn it, repeat the
protocol with reversed roles.

## File Formats
- **State files** are JSON objects with fields:
  - `role`: `"server"` or `"client"`
  - `priv`: base64 encoded private key bytes
  - For servers, also `client_size`, `fpr` and the chosen data structure (`ds`) used to generate the setup message.
- **Blob files** contain a single base64 encoded string representing the
  serialized protobuf message (`ServerSetup`, `Request` or `Response`).

## Security Considerations
- Private keys are stored in plain files. Users should protect these files and
  delete them after use.
- The chosen false positive rate (`fpr`) controls the probability of spurious
  matches. A value of `1e-6` is used by default.
- The data structure can be `gcs` (Golomb Compressed Sets) or `raw`. Using
  `raw` avoids any possibility of false positives at the cost of larger
  messages.
- The protocol assumes semi-honest participants and does not hide the client
  set size.

## Build and Tests
A `Makefile` installs Python dependencies using `uv` and runs the tests with
`pytest`. The tests execute the complete workflow on small example lists to
ensure the scripts work end-to-end.
