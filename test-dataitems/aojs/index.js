import { S3Client, GetObjectCommand } from '@aws-sdk/client-s3'

// S3 configuration from your s3_device.config
const ACCESS_KEY_ID = "";
const SECRET_ACCESS_KEY = "";
const BUCKET_NAME = "offchain-dataitems" // should be this value
// change to your test S3 dataitem ID
const S3_DATAITEM_ID = "yXuCzcOjglttEKRqW7dDYeNy2s_S9eSSFkwzFYRVATg"

const s3Client = new S3Client({
  endpoint: 'http://localhost:8734/~s3@1.0', // s3 device
  region: 'eu-west-2',
  credentials: {
    accessKeyId: ACCESS_KEY_ID,
    secretAccessKey: SECRET_ACCESS_KEY
  },
  forcePathStyle: true 
})

async function downloadAndPostDataItem(dataItemId) {
  try {
    console.log(`Downloading DataItem ${dataItemId}.ans104 from S3`)
    
    const getObjectCommand = new GetObjectCommand({
      Bucket: BUCKET_NAME, 
      Key: `dataitems/${dataItemId}.ans104`
    })
    
    const s3Response = await s3Client.send(getObjectCommand)
    const dataItemBuffer = Buffer.from(await s3Response.Body.transformToByteArray())
    
    console.log(`Downloaded DataItem (${dataItemBuffer.length} bytes)`)
    
    // Post DataItem to AO MU
    console.log('Posting DataItem to MU')
    const muResponse = await fetch('https://mu.ao-testnet.xyz', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/octet-stream',
        'Accept': 'application/json'
      },
      body: dataItemBuffer
    })

    const responseText = await muResponse.text()
    
    if (!muResponse.ok) {
      console.log('MU Error:', responseText)
      throw new Error(`MU registration failed: ${muResponse.status} - ${responseText}`)
    }

    let result
    try {
      result = JSON.parse(responseText)
    } catch {
      result = responseText
    }
    
    console.log('MU Response:', result)
    return result

  } catch (error) {
    console.error('Error:', error.message)
    throw error
  }
}

downloadAndPostDataItem(S3_DATAITEM_ID);