/**
 * Copyright © 2024 Apple Inc. and the Pkl project authors. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
import { glob as _glob } from 'glob'
import * as fs from 'fs/promises'
import { promisify } from 'util'
import * as path from 'path'
import { spawn as _spawn } from 'child_process'

const glob = promisify(_glob);
const spawn = promisify(_spawn);

const testDir = '../pkl/pkl-core/src/test/files/LanguageSnippetTests/input';
const pattern = '**/*.pkl';
const treeSitterCmd = 'node_modules/.bin/tree-sitter';

(async () => {
  await fs.rmdir('test/corpus/snippetTests/', { recursive: true });

  const srcFiles = await glob(`${testDir}/${pattern}`);
  for (const srcFile of srcFiles) {
    const contents = await fs.readFile(srcFile, { encoding: 'utf-8' })
    const testName = srcFile.replace(testDir + '/', '').replace(/.pkl$/, '')
    const output = `================================================================================\n${testName}\n================================================================================\n\n${contents.trim()}\n\n--------------------------------------------------------------------------------\n\n`
    const dest = `test/corpus/snippetTests/${testName}.txt`;
    // const destExists = await fs.
    await fs.mkdir(path.dirname(dest), { recursive: true });
    await fs.writeFile(dest, output, { encoding: 'utf-8' });
  }

  await spawn(treeSitterCmd, ['test', '--update'], { stdio: 'inherit' });
})();
