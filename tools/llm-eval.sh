#!/bin/sh
# SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Evaluate one or more GGUF models as the van-booking intent normalizer.
# For each model: start llama-server with the production flags on a spare
# port, wait for /health, run tools/llm-eval.lisp against it, stop it, and
# keep the report in $OUT/<model>.txt.
#
#   tools/llm-eval.sh /path/to/model-a.gguf [/path/to/model-b.gguf ...]
#
# Environment:
#   LLAMA_SERVER  llama-server binary   (default: llama-server on PATH)
#   PORT          port to listen on     (default: 8099)
#   THREADS       -t for llama-server   (default: nproc)
#   OUT           report directory      (default: ./llm-eval-out)
#   SBCL_OPTS     extra sbcl options
set -eu

LLAMA_SERVER=${LLAMA_SERVER:-llama-server}
PORT=${PORT:-8099}
THREADS=${THREADS:-$(nproc)}
OUT=${OUT:-./llm-eval-out}
HERE=$(cd "$(dirname "$0")" && pwd)
mkdir -p "$OUT"

for model in "$@"; do
  name=$(basename "$model" .gguf)
  echo "=== $name"
  "$LLAMA_SERVER" --host 127.0.0.1 --port "$PORT" -m "$model" -t "$THREADS" \
      -c 2048 --parallel 1 --cache-reuse 256 --jinja --reasoning-budget 0 \
      --chat-template-kwargs '{"enable_thinking":false}' --no-webui \
      > "$OUT/$name.server.log" 2>&1 &
  pid=$!
  for i in $(seq 1 120); do
    if curl -sf "http://127.0.0.1:$PORT/health" >/dev/null 2>&1; then break; fi
    sleep 1
  done
  if ! curl -sf "http://127.0.0.1:$PORT/health" >/dev/null 2>&1; then
    echo "llama-server did not come up for $name; see $OUT/$name.server.log"
    kill "$pid" 2>/dev/null || true
    continue
  fi
  start=$(date +%s)
  VAN_LLM_URL="http://127.0.0.1:$PORT/v1/chat/completions" \
    sbcl --dynamic-space-size 4Gb --control-stack-size 16Mb \
         --no-userinit --no-sysinit --non-interactive ${SBCL_OPTS:-} \
         --load "$HERE/llm-eval.lisp" > "$OUT/$name.txt" 2>&1 || true
  echo "took $(( $(date +%s) - start ))s; report in $OUT/$name.txt"
  tail -3 "$OUT/$name.txt"
  kill "$pid" 2>/dev/null || true
  wait "$pid" 2>/dev/null || true
done
