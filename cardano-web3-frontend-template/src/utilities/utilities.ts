import { Lucid, TxComplete, TxHash } from "lucid-cardano";

export const signAndSubmitTx = async (tx: TxComplete): Promise<TxHash | null> => {
    try {
        const signedTx = await tx.sign().complete();
        const txHash = await signedTx.submit();
        console.log(`Transaction submitted: ${txHash}`);
        alert(`Transaction submitted: ${txHash}`);
        return txHash;
    } catch (error) {
        if (error instanceof Error) {
            if (error.message.includes('user declined')) {
                console.error('Transaction signing declined by user');
                alert('Transaction was declined. Please try again and approve the transaction in your wallet.');
            } else {
                console.error('Error during transaction signing or submission:', error.message);
                alert(`An error occurred: ${error.message}`);
            }
        } else {
            console.error('An unknown error occurred:', error);
            alert('An unknown error occurred. Please try again.');
        }
        return null;
    }
};

export const safeStringToBigInt = (r: string): bigint | undefined => {
    const parsed = BigInt(Number(r));
    if (Number.isNaN(parsed)) return;
    return parsed;
};

export const findUTxO = async (lucid: Lucid, ref: string) => {
    const [txH, ix] = ref.split("#");
    const utxos = await lucid.utxosByOutRef([
        {
            txHash: txH,
            outputIndex: Number(ix),
        },
    ]);
    return utxos[0];
};
