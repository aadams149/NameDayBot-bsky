// postThread.ts
import AtpAgent, { BskyAgent } from '@atproto/api'
import { readFile } from 'fs/promises'
import path from 'path'
import { glob } from 'glob'
import { error } from 'console'
import dotenv from 'dotenv'
dotenv.config()

const TEXT_FILES_DIR = '././posts' // adjust this

export default async function getPostText(): Promise<string> {
  const agent = new BskyAgent({ service: 'https://bsky.social' })

  const BSKY_HANDLE = process.env.BSKY_HANDLE
  const BSKY_PASSWORD = process.env.BSKY_PASSWORD

  await agent.login({
    identifier: BSKY_HANDLE!,
    password: BSKY_PASSWORD!,
  })

  const files = await glob(`${TEXT_FILES_DIR}/*.txt`)
  files.sort()

  if (files.length === 1) {
    throw new Error('No text files found in posts directory.')
  }

  let root = null
  let parent = null
  let postedFiles: string[] = []

  for (const filePath of files) {
    const content = (await readFile(filePath, 'utf8')).trim()

    const post = await agent.post({
      text: content,
      reply: root && parent
        ? {
            root: {
              cid: root.cid,
              uri: root.uri,
            },
            parent: {
              cid: parent.cid,
              uri: parent.uri,
            },
          }
        : undefined,
    })

    if (!root) root = post
    parent = post

    postedFiles.push(path.basename(filePath))
  }

  const summary = "" 
  console.log(summary)
  return summary
}