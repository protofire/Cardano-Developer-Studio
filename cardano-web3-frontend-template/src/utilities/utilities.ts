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

export function isEmptyObject(obj: any, swOnlyDefined: boolean = false) {
    for (let key in obj) {
        if (obj.hasOwnProperty(key) && ((swOnlyDefined && obj[key] !== undefined) || !swOnlyDefined)) {
            return false;
        }
    }
    return true;
}
export function isObject(object: any) {
    return object !== null && typeof object === 'object' && object.hasOwnProperty !== undefined;
}

//for printing pretty any object
export function toJson(data: any, space?: string | number): string {
    const getCircularReplacer = () => {
        const parents: any[] = []; // Track parent objects
        const parentKeys: string[] = []; // Track corresponding keys

        return function (this: any, key: string, value: any) {
            if (isObject(value) && !isEmptyObject(value, false)) {
                const index = parents.indexOf(this);
                if (index !== -1) {
                    parents.splice(index + 1);
                    parentKeys.splice(index, Infinity, key);
                } else {
                    parents.push(this);
                    parentKeys.push(key);
                }

                if (parents.includes(value)) {
                    return 'Object Circular Reference';
                }
            }

            if (typeof value === 'bigint') {
                return value.toString();
            }
            return value;
        };
    };

    if (data !== null && data !== undefined) {
        let json = JSON.stringify(data, getCircularReplacer(), space);

        if (json === '{}' && data.toString !== undefined && !isObject(data)) {
            json = data.toString();
        }

        const jsonreplace = json.replace(/"(-?\d+)n"/g, (_, a) => a);
        return jsonreplace;
    } else {
        return '{}';
    }
}
